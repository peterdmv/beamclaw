---
name: example-skill
description: An example skill showing the SKILL.md format
metadata: {"beamclaw": {"emoji": "book", "always": true}}
---

# Example Skill

This is a bundled example skill that demonstrates the SKILL.md format.

## What This Skill Does

Nothing — it's a reference implementation. Use it as a template when
creating your own skills.

## Creating Your Own Skills

1. Create a directory under `~/.beamclaw/skills/your-skill-name/`
2. Add a `SKILL.md` file with frontmatter and content
3. The frontmatter must include at least a `name:` field
4. Optional: add `metadata:` with JSON for requirements and install specs

## Frontmatter Fields

- `name:` — Skill identifier (required)
- `description:` — One-line description of when to use this skill
- `homepage:` — URL for more information
- `metadata:` — JSON object with BeamClaw-specific configuration

## Metadata Structure

```json
{
  "beamclaw": {
    "emoji": "wrench",
    "always": false,
    "requires": {
      "bins": ["jq", "curl"],
      "env": ["API_KEY"],
      "os": ["linux", "darwin"]
    },
    "install": [
      {"kind": "apt", "package": "jq"},
      {"kind": "brew", "formula": "jq"}
    ]
  }
}
```
