# 0003 – BGG XML API: token, limits, terms and approved-scope fit

Research for issue #24. Date: 2026-10-14.

## Sources and access caveat

All BGG pages returned HTTP 403 to automated clients, so the text below was read from Internet Archive snapshots of the official BGG pages (snapshot dates in brackets). Re-verify against the live pages while logged in before relying on any detail.

- Using the XML API, "Version date 2025-07-02" [snapshot 2025-12-03]: <https://boardgamegeek.com/using_the_xml_api>
- XML API Terms of Use [2025-12-15]: <https://boardgamegeek.com/wiki/page/XML_API_Terms_of_Use>
- BGG_XML_API2 wiki [2025-12-30]: <https://boardgamegeek.com/wiki/page/BGG_XML_API2>
- BGG_XML_API wiki (rate limit) [2025-12-30]: <https://boardgamegeek.com/wiki/page/BGG_XML_API>
- Approved application "Dominion News": local `bgg_app.md` (status: approved, non-commercial, not public).

The Geek Tools News forum (where changes are announced) could not be read from the archive; no announcement was verified.

## Findings

### Token requirement and usage
- Registration and authorization are required for nearly all XML API use (using_the_xml_api).
- Tokens are created on <https://boardgamegeek.com/applications> ("Tokens" next to the application).
- Requests must be HTTPS to `boardgamegeek.com` (no `www`) with header `Authorization: Bearer <token>` ("Bearer", a space, no colon).
- Unauthenticated calls fail: `xmlapi2/forumlist` returned HTTP 401 without a token.
- Lifetime/regeneration: the page says tokens are not currently required to be refreshed, "this may change". No expiry period is published. Regeneration is possible from the Tokens page; a captured/leaked token may need regenerating. Keep the token out of Git (it is in an untracked `.bgg` file).
- Calls should be made server-side with results cached.

### Rate limits
- No exact published limits: "We are still determining exact usage limits"; keep requests to a minimum; limits may depend on license.
- BGG_XML_API wiki: BGG throttles with HTTP 500/503; "a 5-second wait between requests seems to suffice" (informal guidance).
- `thread` supports `minarticleid`, `minarticledate`, `count` (max 1000), which allow incremental pulls. `forum` pages are 50 threads, sorted by most recent post.

### Terms on storing data
- The license is a worldwide, non-exclusive, royalty-free license to "reproduce and display" the data, including User Submissions, "solely for strictly non-commercial purposes and solely as permitted by the XML API". BGG can terminate it at any time for any reason.
- Data may not be modified "in any way"; BGG must be credited by name as source; public-facing uses must show the "Powered by BGG" logo linking to BGG.
- **Use of the XML API or any site data to train an AI/LLM system is strictly prohibited.**
- Usage must not interfere with BGG servers; BGG may monitor activity.
- Third-party services that give other applications access to the data are strictly prohibited.
- No explicit rule on retention duration or caching was found; the guide recommends caching.

### Fit with the approved scope
Approved application: non-commercial, not available to the public, no data sharing, daily requests, Dominion only, "not a massive data lake".

| Plan | Fit |
|---|---|
| Daily/monthly crawl of Dominion forums for private analysis | Covered. |
| Retaining a historical evidence store privately | Plausibly covered (private analysis; terms don't forbid retention), but the application says no massive data lake; Dominion-only and modest volume keeps it consistent. Confirm with BGG if in doubt. |
| Public recommender displaying BGG-derived content | **Not covered.** The application says "Is Available to the Public: No" and no data sharing. A public app needs an application update/new approval, the Powered by BGG logo, credit, and unmodified display of any User Submission shown. It must stay non-commercial or get a commercial license. |
| Quarterly LLM semantic analysis of BGG posts | **Risk.** The terms prohibit using the API/data to "train" AI/LLMs. Prompting a model on posts for classification is not clearly "training", but it is not clearly allowed; a hosted model also sends BGG data to a third party. Get explicit BGG clarification first; until then keep BGG text out of LLM batches (Reddit evidence unaffected). |
| Publishing derived scores/claims (no raw text) | Unclear. "May not modify the data" and the no-sharing statement suggest asking BGG first. |

Tension with repo guidance: retaining all authors' posts and feeding them to an offline LLM batch must be gated on BGG's answer. No technical support is offered by BGG; contact via the application or the Geek Tools guild.

### Vote/thumb field on forum posts
- The `thread` parameters (id, minarticleid, minarticledate, count, username [unsupported]) and wiki text document no vote, thumb, or rating field for forum articles. The wiki does not describe response XML.
- Not verified against a live response (the token was not used in this research). Consistent with `R/bgg_crawler.R`, which captures no vote field. Treat votes as unknown until a live sample proves otherwise; do not scrape them from HTML (private endpoints are unlicensed).

## Recommended actions
1. Keep BGG crawling private, Dominion-only, daily/monthly, ≥5 s between requests, incremental via `minarticleid`/`minarticledate`, Bearer header, cached.
2. Before any public recommender or LLM batch using BGG text, ask BGG (update the application) and get written approval.
3. Add "Powered by BGG" and credit in any eventual public UI; do not alter displayed BGG text.
4. Record the vote field as unknown in the evidence model.
5. Re-check live terms periodically; BGG can change them by posting on the page.
