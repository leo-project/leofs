"""Language detection and routing using fasttext."""

import structlog
from pathlib import Path
from typing import Optional

from ..config import settings
from ..exceptions import PermanentError

logger = structlog.get_logger()

# Lazy load fasttext to avoid import errors if model not available
_fasttext_model = None


def _get_model():
    """Lazy load fasttext model."""
    global _fasttext_model
    if _fasttext_model is None:
        try:
            import fasttext

            model_path = Path(settings.fasttext_model_path)
            if not model_path.exists():
                raise PermanentError(
                    f"fasttext model not found: {model_path}. "
                    "Download from: https://dl.fbaipublicfiles.com/fasttext/supervised-models/lid.176.ftz"
                )

            # Suppress fasttext warnings about deprecated model loading
            fasttext.FastText.eprint = lambda x: None
            _fasttext_model = fasttext.load_model(str(model_path))
            logger.info("fasttext_model_loaded", path=str(model_path))
        except ImportError:
            raise PermanentError(
                "fasttext library not installed. Run: pip install fasttext"
            )

    return _fasttext_model


class LanguageResult:
    """Result of language detection."""

    def __init__(
        self, language: str, confidence: float, is_supported: bool, sample_chars: int
    ):
        self.language = language
        self.confidence = confidence
        self.is_supported = is_supported
        self.sample_chars = sample_chars

    def __repr__(self) -> str:
        return (
            f"LanguageResult(language='{self.language}', "
            f"confidence={self.confidence:.3f}, "
            f"is_supported={self.is_supported})"
        )


class LanguageRouter:
    """Language detection and routing using fasttext.

    Uses fasttext's pre-trained language identification model (lid.176.ftz)
    to detect the language of input text and route to appropriate processing.

    Attributes:
        supported_languages: Set of language codes to process (e.g., {"ja", "en"})
        detection_chars: Number of characters to use for detection
        confidence_threshold: Minimum confidence score to accept detection
    """

    def __init__(
        self,
        supported_languages: Optional[set[str]] = None,
        detection_chars: Optional[int] = None,
        confidence_threshold: Optional[float] = None,
    ):
        """Initialize LanguageRouter.

        Args:
            supported_languages: Set of ISO 639-1 language codes to support.
                                 Defaults to settings.supported_languages.
            detection_chars: Number of characters to sample for detection.
                            Defaults to settings.language_detection_chars.
            confidence_threshold: Minimum confidence to accept.
                                  Defaults to settings.language_confidence_threshold.
        """
        self.supported_languages = (
            supported_languages
            if supported_languages is not None
            else set(settings.supported_languages)
        )
        self.detection_chars = (
            detection_chars
            if detection_chars is not None
            else settings.language_detection_chars
        )
        self.confidence_threshold = (
            confidence_threshold
            if confidence_threshold is not None
            else settings.language_confidence_threshold
        )

        logger.debug(
            "language_router_initialized",
            supported_languages=list(self.supported_languages),
            detection_chars=self.detection_chars,
            confidence_threshold=self.confidence_threshold,
        )

    def detect(self, text: str) -> LanguageResult:
        """Detect the language of the input text.

        Uses the first N characters (configured by detection_chars) for detection.
        Newlines are replaced with spaces as recommended by fasttext.

        Args:
            text: The text to analyze

        Returns:
            LanguageResult with detected language, confidence, and support status
        """
        if not text or not text.strip():
            logger.warning("language_detection_empty_text")
            return LanguageResult(
                language="unknown", confidence=0.0, is_supported=False, sample_chars=0
            )

        # Sample first N characters
        sample = text[: self.detection_chars]
        sample_chars = len(sample)

        # fasttext recommends replacing newlines with spaces
        sample = sample.replace("\n", " ").replace("\r", " ")
        # Remove multiple spaces
        sample = " ".join(sample.split())

        model = _get_model()

        # Predict language (k=1 for top prediction only)
        predictions = model.predict(sample, k=1)
        # predictions format: (('__label__ja',), array([0.95]))

        raw_label = predictions[0][0]
        confidence = float(predictions[1][0])

        # Extract language code from label (e.g., "__label__ja" -> "ja")
        language = raw_label.replace("__label__", "")

        is_supported = (
            language in self.supported_languages
            and confidence >= self.confidence_threshold
        )

        result = LanguageResult(
            language=language,
            confidence=confidence,
            is_supported=is_supported,
            sample_chars=sample_chars,
        )

        logger.debug(
            "language_detected",
            language=language,
            confidence=confidence,
            is_supported=is_supported,
            sample_chars=sample_chars,
        )

        return result

    def is_supported(self, language: str) -> bool:
        """Check if a language code is in the supported set.

        Args:
            language: ISO 639-1 language code (e.g., "ja", "en")

        Returns:
            True if the language is supported
        """
        return language in self.supported_languages

    def should_process(self, text: str) -> tuple[bool, LanguageResult]:
        """Determine if text should be processed based on language.

        Convenience method that combines detection and support checking.

        Args:
            text: The text to analyze

        Returns:
            Tuple of (should_process, LanguageResult)
        """
        result = self.detect(text)
        return result.is_supported, result
