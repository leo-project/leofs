import socket
import sys

UDP_IP = "127.0.0.1"
UDP_PORT = 5000
MESSAGE = b"test-bucket|test-key/path/image.jpg"

print(f"Sending UDP packet to {UDP_IP}:{UDP_PORT}...")
try:
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.sendto(MESSAGE, (UDP_IP, UDP_PORT))
    print("Sent.")
except Exception as e:
    print(f"Error: {e}")
