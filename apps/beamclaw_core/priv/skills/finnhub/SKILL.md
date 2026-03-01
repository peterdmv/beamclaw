---
name: finnhub
description: Fetches real-time financial news headlines and summaries from Finnhub.io.
metadata: {"beamclaw": {"requires": {"env": ["FINNHUB_TOKEN"], "bins": ["curl", "jq"]}}}
---

# Finnhub Market News

Fetch the latest financial news headlines from Finnhub.io using the Market News endpoint.

## API Details

- **Base URL:** `https://finnhub.io/api/v1/news`
- **Auth:** Pass the token as a query parameter `token=$FINNHUB_TOKEN`
- **Categories:** `general`, `forex`, `crypto`, `merger`

## How to Execute

### Fetch general market news (default)

```bash
curl -s "https://finnhub.io/api/v1/news?category=general&token=$FINNHUB_TOKEN" \
  | jq '[.[:5][] | {id, time: (.datetime | todate), headline, summary, url}]'
```

### Fetch news by category

Replace `CATEGORY` with one of: `general`, `forex`, `crypto`, `merger`.

```bash
curl -s "https://finnhub.io/api/v1/news?category=CATEGORY&token=$FINNHUB_TOKEN" \
  | jq '[.[:5][] | {id, time: (.datetime | todate), headline, summary, url}]'
```

### Deduplicate with min_id

To avoid repeating headlines already shown, pass the highest `id` from the previous response as `minId`:

```bash
curl -s "https://finnhub.io/api/v1/news?category=general&minId=MIN_ID&token=$FINNHUB_TOKEN" \
  | jq '[.[:5][] | {id, time: (.datetime | todate), headline, summary, url}]'
```

## Rules

- Always limit output to **5 headlines** (already handled by `jq` slicing above).
- When the user asks for "crypto news", "forex news", etc., use the matching `category` parameter.
- Default to `category=general` if no specific category is mentioned.
- If the user asks for follow-up or "any new headlines", use `minId` set to the highest `id` from the previous result.
- Present results as a readable list with headline, time, and summary. Include the URL for each story.
