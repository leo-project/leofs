"""Text preprocessing for different languages."""

import re
import unicodedata
import structlog

logger = structlog.get_logger()


class TextPreprocessor:
    """Text preprocessing with language-specific handling.

    Performs text normalization and cleanup operations:
    - Unicode normalization (NFKC)
    - Whitespace normalization
    - Control character removal
    - Language-specific processing (e.g., Japanese full-width conversion)
    """

    # Control characters to remove (excluding newlines and tabs)
    CONTROL_CHARS_PATTERN = re.compile(r"[\x00-\x08\x0b\x0c\x0e-\x1f\x7f-\x9f]")

    # Multiple whitespace pattern
    MULTI_SPACE_PATTERN = re.compile(r"[ \t]+")
    MULTI_NEWLINE_PATTERN = re.compile(r"\n{3,}")

    def __init__(
        self, normalize_unicode: bool = True, remove_control_chars: bool = True
    ):
        """Initialize TextPreprocessor.

        Args:
            normalize_unicode: Apply NFKC Unicode normalization
            remove_control_chars: Remove control characters
        """
        self.normalize_unicode = normalize_unicode
        self.remove_control_chars = remove_control_chars

    def process(self, text: str, language: str = "en") -> str:
        """Process text with language-specific handling.

        Args:
            text: Input text to process
            language: ISO 639-1 language code (e.g., "ja", "en")

        Returns:
            Processed text
        """
        if not text:
            return ""

        original_length = len(text)

        # Step 1: Unicode normalization (NFKC)
        if self.normalize_unicode:
            text = unicodedata.normalize("NFKC", text)

        # Step 2: Remove control characters
        if self.remove_control_chars:
            text = self.CONTROL_CHARS_PATTERN.sub("", text)

        # Step 3: Language-specific processing
        if language == "ja":
            text = self._process_japanese(text)
        elif language == "en":
            text = self._process_english(text)
        else:
            # Default processing for other languages
            text = self._process_default(text)

        # Step 4: Common cleanup
        text = self._common_cleanup(text)

        processed_length = len(text)
        logger.debug(
            "text_preprocessed",
            language=language,
            original_length=original_length,
            processed_length=processed_length,
            reduction_pct=(
                round((1 - processed_length / original_length) * 100, 2)
                if original_length > 0
                else 0
            ),
        )

        return text

    def _process_japanese(self, text: str) -> str:
        """Japanese-specific text processing.

        - NFKC already handles full-width to half-width for numbers/ASCII
        - Normalize Japanese-specific whitespace
        """
        # Replace Japanese full-width space with regular space
        text = text.replace("\u3000", " ")

        # Normalize Japanese punctuation spacing (optional)
        # Keep natural Japanese text flow

        return text

    def _process_english(self, text: str) -> str:
        """English-specific text processing.

        - Standard whitespace normalization
        """
        return text

    def _process_default(self, text: str) -> str:
        """Default text processing for unsupported languages."""
        return text

    def _common_cleanup(self, text: str) -> str:
        """Common cleanup applied to all languages.

        - Normalize multiple spaces to single space
        - Normalize multiple newlines (3+ -> 2)
        - Strip leading/trailing whitespace
        """
        # Normalize multiple spaces (but not newlines)
        text = self.MULTI_SPACE_PATTERN.sub(" ", text)

        # Normalize excessive newlines
        text = self.MULTI_NEWLINE_PATTERN.sub("\n\n", text)

        # Strip leading/trailing whitespace
        text = text.strip()

        return text


class HTMLCleaner:
    """Optional HTML/Markdown tag removal.

    Use this when processing HTML or Markdown content that needs
    to be converted to plain text before chunking.
    """

    # HTML tag pattern
    HTML_TAG_PATTERN = re.compile(r"<[^>]+>")

    # Markdown patterns
    MD_HEADER_PATTERN = re.compile(r"^#{1,6}\s*", re.MULTILINE)
    MD_LINK_PATTERN = re.compile(r"\[([^\]]+)\]\([^)]+\)")
    MD_IMAGE_PATTERN = re.compile(r"!\[([^\]]*)\]\([^)]+\)")
    MD_BOLD_PATTERN = re.compile(r"\*\*([^*]+)\*\*|__([^_]+)__")
    MD_ITALIC_PATTERN = re.compile(r"\*([^*]+)\*|_([^_]+)_")
    MD_CODE_BLOCK_PATTERN = re.compile(r"```[\s\S]*?```")
    MD_INLINE_CODE_PATTERN = re.compile(r"`([^`]+)`")

    def clean_html(self, text: str) -> str:
        """Remove HTML tags from text.

        Args:
            text: Text potentially containing HTML tags

        Returns:
            Text with HTML tags removed
        """
        return self.HTML_TAG_PATTERN.sub("", text)

    def clean_markdown(self, text: str) -> str:
        """Convert Markdown to plain text.

        Args:
            text: Markdown-formatted text

        Returns:
            Plain text without Markdown formatting
        """
        # Remove code blocks first (preserve content inside)
        text = self.MD_CODE_BLOCK_PATTERN.sub("", text)

        # Remove images (keep alt text)
        text = self.MD_IMAGE_PATTERN.sub(r"\1", text)

        # Convert links to just text
        text = self.MD_LINK_PATTERN.sub(r"\1", text)

        # Remove bold/italic markers (keep content)
        text = self.MD_BOLD_PATTERN.sub(r"\1\2", text)
        text = self.MD_ITALIC_PATTERN.sub(r"\1\2", text)

        # Remove inline code markers
        text = self.MD_INLINE_CODE_PATTERN.sub(r"\1", text)

        # Remove header markers
        text = self.MD_HEADER_PATTERN.sub("", text)

        return text
