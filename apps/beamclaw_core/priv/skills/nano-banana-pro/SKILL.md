---
name: nano-banana-pro
description: Generate or edit images via Gemini image models on OpenRouter (Nano Banana Pro).
homepage: https://openrouter.ai/google/gemini-2.5-flash-image
metadata: {"beamclaw": {"emoji": "🍌", "requires": {"bins": ["uv"], "env": ["OPENROUTER_API_KEY"]}, "install": [{"kind": "brew", "formula": "uv"}]}}
---

# Nano Banana Pro (Gemini Image via OpenRouter)

Use the bundled script to generate or edit images.

Generate

```bash
uv run {baseDir}/scripts/generate_image.py --prompt "your image description" --filename "output.png" --resolution 1K
```

Edit (single image)

```bash
uv run {baseDir}/scripts/generate_image.py --prompt "edit instructions" --filename "output.png" -i "/path/in.png" --resolution 2K
```

Multi-image composition (up to 14 images)

```bash
uv run {baseDir}/scripts/generate_image.py --prompt "combine these into one scene" --filename "output.png" -i img1.png -i img2.png -i img3.png
```

API key

- `OPENROUTER_API_KEY` env var (same key used for LLM providers)

Models

- Default: `google/gemini-2.5-flash-image` (fast, cheap). No extra flags needed.
- Pro: `google/gemini-3-pro-image-preview` (higher quality). Use `--model google/gemini-3-pro-image-preview` when the user asks for pro/high-quality output.

Notes

- Resolutions: `1K` (default), `2K`, `4K`.
- Use timestamps in filenames: `yyyy-mm-dd-hh-mm-ss-name.png`.
- The script prints a `MEDIA:` line for auto-attachment on supported chat providers.
- Do not read the image back; report the saved path only.
- IMPORTANT: After running generate_image.py, do NOT verify the output file
  with ls, cat, or any other command. The script confirms success by printing
  the saved path — just report the result to the user immediately.
- When editing a user-provided image, use the file path from the `[Attached image saved to ...]` line in the chat message (e.g., `-i /tmp/bc_attach_12345.jpg`).
