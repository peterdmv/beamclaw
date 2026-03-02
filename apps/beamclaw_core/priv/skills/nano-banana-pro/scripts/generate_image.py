#!/usr/bin/env python3
# /// script
# requires-python = ">=3.10"
# dependencies = [
#     "pillow>=10.0.0",
# ]
# ///
"""
Generate images using Gemini image models on OpenRouter.

Usage:
    uv run generate_image.py --prompt "your image description" --filename "output.png" [--resolution 1K|2K|4K] [--api-key KEY]

Multi-image editing (up to 14 images):
    uv run generate_image.py --prompt "combine these images" --filename "output.png" -i img1.png -i img2.png -i img3.png
"""

import argparse
import base64
import json
import os
import sys
import urllib.request
from io import BytesIO
from pathlib import Path


def get_api_key(provided_key: str | None) -> str | None:
    """Get API key from argument first, then environment."""
    if provided_key:
        return provided_key
    return os.environ.get("OPENROUTER_API_KEY")


def encode_image_base64(image_path: str) -> tuple[str, str]:
    """Read an image file and return (mime_type, base64_data)."""
    path = Path(image_path)
    suffix = path.suffix.lower()
    mime_map = {
        ".png": "image/png",
        ".jpg": "image/jpeg",
        ".jpeg": "image/jpeg",
        ".gif": "image/gif",
        ".webp": "image/webp",
    }
    mime_type = mime_map.get(suffix, "image/png")
    with open(path, "rb") as f:
        data = base64.b64encode(f.read()).decode("ascii")
    return mime_type, data


def main():
    parser = argparse.ArgumentParser(
        description="Generate images using Gemini image models on OpenRouter (Nano Banana Pro)"
    )
    parser.add_argument(
        "--prompt", "-p",
        required=True,
        help="Image description/prompt"
    )
    parser.add_argument(
        "--filename", "-f",
        required=True,
        help="Output filename (e.g., sunset-mountains.png)"
    )
    parser.add_argument(
        "--input-image", "-i",
        action="append",
        dest="input_images",
        metavar="IMAGE",
        help="Input image path(s) for editing/composition. Can be specified multiple times (up to 14 images)."
    )
    parser.add_argument(
        "--resolution", "-r",
        choices=["1K", "2K", "4K"],
        default="1K",
        help="Output resolution: 1K (default), 2K, or 4K"
    )
    parser.add_argument(
        "--api-key", "-k",
        help="OpenRouter API key (overrides OPENROUTER_API_KEY env var)"
    )
    parser.add_argument(
        "--model", "-m",
        default="google/gemini-2.5-flash-image",
        help="OpenRouter model ID (default: google/gemini-2.5-flash-image)"
    )

    args = parser.parse_args()

    # Get API key
    api_key = get_api_key(args.api_key)
    if not api_key:
        print("Error: No API key provided.", file=sys.stderr)
        print("Please either:", file=sys.stderr)
        print("  1. Provide --api-key argument", file=sys.stderr)
        print("  2. Set OPENROUTER_API_KEY environment variable", file=sys.stderr)
        sys.exit(1)

    # Import PIL here after checking API key to avoid slow import on error
    from PIL import Image as PILImage

    # Set up output path
    output_path = Path(args.filename)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    # Build message content parts
    content_parts = []
    output_resolution = args.resolution

    # Load input images if provided (up to 14 supported by Nano Banana Pro)
    if args.input_images:
        if len(args.input_images) > 14:
            print(f"Error: Too many input images ({len(args.input_images)}). Maximum is 14.", file=sys.stderr)
            sys.exit(1)

        max_input_dim = 0
        for img_path in args.input_images:
            try:
                # Get dimensions for auto-resolution
                img = PILImage.open(img_path)
                width, height = img.size
                max_input_dim = max(max_input_dim, width, height)
                img.close()

                # Encode as base64 data URL
                mime_type, b64_data = encode_image_base64(img_path)
                content_parts.append({
                    "type": "image_url",
                    "image_url": {
                        "url": f"data:{mime_type};base64,{b64_data}"
                    }
                })
                print(f"Loaded input image: {img_path}")
            except Exception as e:
                print(f"Error loading input image '{img_path}': {e}", file=sys.stderr)
                sys.exit(1)

        # Auto-detect resolution from largest input if not explicitly set
        if args.resolution == "1K" and max_input_dim > 0:  # Default value
            if max_input_dim >= 3000:
                output_resolution = "4K"
            elif max_input_dim >= 1500:
                output_resolution = "2K"
            else:
                output_resolution = "1K"
            print(f"Auto-detected resolution: {output_resolution} (from max input dimension {max_input_dim})")

    # Add text prompt
    content_parts.append({"type": "text", "text": args.prompt})

    if args.input_images:
        img_count = len(args.input_images)
        print(f"Processing {img_count} image{'s' if img_count > 1 else ''} with resolution {output_resolution}...")
    else:
        print(f"Generating image with resolution {output_resolution}...")

    # Build OpenRouter request
    request_body = {
        "model": args.model,
        "messages": [
            {"role": "user", "content": content_parts}
        ],
        "modalities": ["image", "text"],
        "image_config": {"image_size": output_resolution},
    }

    data = json.dumps(request_body).encode("utf-8")
    req = urllib.request.Request(
        "https://openrouter.ai/api/v1/chat/completions",
        data=data,
        headers={
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
        },
        method="POST",
    )

    try:
        with urllib.request.urlopen(req, timeout=120) as resp:
            result = json.loads(resp.read().decode("utf-8"))
    except urllib.error.HTTPError as e:
        body = e.read().decode("utf-8", errors="replace")
        print(f"Error: OpenRouter API returned {e.code}: {body}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error generating image: {e}", file=sys.stderr)
        sys.exit(1)

    # Parse response
    image_saved = False
    try:
        message = result["choices"][0]["message"]

        # Print any text content
        if message.get("content"):
            print(f"Model response: {message['content']}")

        # Extract images from response
        images = message.get("images", [])
        for img_entry in images:
            image_url = img_entry.get("image_url", {}).get("url", "")
            if not image_url:
                continue

            # Parse base64 data URL: "data:image/png;base64,..."
            if image_url.startswith("data:"):
                header, b64 = image_url.split(",", 1)
                image_data = base64.b64decode(b64)
            else:
                # Fallback: treat as raw base64
                image_data = base64.b64decode(image_url)

            image = PILImage.open(BytesIO(image_data))

            # Ensure RGB mode for PNG
            if image.mode == "RGBA":
                rgb_image = PILImage.new("RGB", image.size, (255, 255, 255))
                rgb_image.paste(image, mask=image.split()[3])
                rgb_image.save(str(output_path), "PNG")
            elif image.mode == "RGB":
                image.save(str(output_path), "PNG")
            else:
                image.convert("RGB").save(str(output_path), "PNG")
            image_saved = True
            break  # Save first image only

    except (KeyError, IndexError) as e:
        print(f"Error parsing response: {e}", file=sys.stderr)
        print(f"Response: {json.dumps(result, indent=2)}", file=sys.stderr)
        sys.exit(1)

    if image_saved:
        full_path = output_path.resolve()
        print(f"\nImage saved: {full_path}")
        # OpenClaw parses MEDIA tokens and will attach the file on supported providers.
        print(f"MEDIA: {full_path}")
    else:
        print("Error: No image was generated in the response.", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
