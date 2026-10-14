# Research: Reddit access, rate limits and terms

Ticket: `gstephan30/reddit_kotw#23`, child of Wayfinder map `#22`. Researched 2026-10-14.

## Question

What does Reddit currently require to read r/dominion posts and comments at monthly cadence? Do the unauthenticated `.json` endpoints used by `subreddit.R` still work, is a registered Data API app/OAuth required, what are the rate limits, and what do Reddit's terms say about storing and using public content?

## Short answer

1. **Unauthenticated `.json` is not reliable.** A live request to the same search endpoint `subreddit.R` uses returned `HTTP 403 Blocked` (see Observed behavior). Plan on OAuth.
2. **Written policy requires the Data API route.** Access must use the "Access Info" (e.g. OAuth token) from the Developer Documentation, with accurate contact information and an unmasked user agent.
3. **Numeric rate limit: unconfirmed.** The Data API Terms leave limits to Reddit's "sole discretion". The only first-party number retrieved is 60 requests/minute from Reddit's archived API wiki (legacy). Current docs were not reachable.
4. **Retention is bounded by an "approved use case"**, with no fixed time limit. User Content is owned by its authors; no AI/ML training use without rightsholder permission; commercial use needs a separate agreement.

## Verified policy (written sources)

Source A: Reddit Data API Terms, `https://redditinc.com/policies/data-api-terms` (fetched live; the page's version list shows July 1, 2026 as the newest User Agreement revision; the Data API Terms page itself carries no visible effective date in the fetched text).

| Topic | What the source says | Section |
|---|---|---|
| Registration | Access requires providing identification information (e.g. contact details), kept up to date and accurate. | 1.3 |
| Authentication | You will "only access ... Data APIs using Access Info described in the Developer Documentation" and must use the Access Info provided (e.g. the OAuth token); no misrepresenting or masking the user agent or OAuth identity. | 2.8 |
| Rate limits | Reddit "may set and enforce limits ... in our sole discretion". No number appears in the Terms. | 2.9 |
| Circumventing limits | Must not "circumvent or exceed limitations on calls and use of the Data APIs as outlined in the Developer Documentation" or make excessive or abusive use. Reddit may permanently block access. | 3.2 |
| Fees / commercial | Reddit may charge fees. Commercial purposes, "research in excess of rate limits", or any use not expressly permitted need a separate agreement. | 3.1 |
| Selling / revenue | No selling, leasing or sublicensing the Data APIs or deriving revenue from them without express written approval. | 3.2 |
| Content ownership and license | User Content is owned by users. Reddit licenses copying and displaying it "solely as necessary to develop, deploy, distribute, and run your App to your App Users"; no modification except formatting; author-side restrictions (all rights reserved, Creative Commons) must be honored. | 2.4 |
| AI/ML | No right to use User Content for training a machine learning or AI model without the express permission of rightsholders. | 2.4, 3.2 |
| Retention | Must not "use or retain any User Content, Materials, or data accessed through the Data APIs beyond your approved use case", and must "immediately delete any data not required for it". No fixed duration is given. | 3.2 |
| Removals | Removal notices under the Reddit Rules must be escalated to Reddit as needed via its request form. The Terms state no explicit duty to mirror live deletions. | 2.5 |
| Privacy disclosure | Must disclose how you collect, use, store and disclose data from app users and visitors. | 2.6 |
| Attribution | Display "any attribution(s) required by Reddit as described in the Developer Documentation". Reddit's wordmark may be used only as "[name] for Reddit". | 4.2 |
| Changes | Reddit may change, suspend or discontinue any aspect of the Data APIs at any time. | 5.2 |

Source B: Reddit Developer Terms, `https://redditinc.com/policies/developer-terms` (fetched live). Only the later sections (10.x: changes, governing law, general) came back. Section 10.2 says Reddit may change the Developer Terms or Developer Documentation, and that reviewing them is the developer's responsibility.

Source C: Reddit's archived API wiki, `https://github.com/reddit-archive/reddit/wiki/API` (raw text read from `raw.githubusercontent.com/wiki/reddit-archive/reddit/API.md`). This is first-party but **archived/legacy**:
- "Clients must authenticate with OAuth2."
- OAuth clients "may make up to 60 requests per minute"; monitor `X-Ratelimit-Used`, `X-Ratelimit-Remaining`, `X-Ratelimit-Reset`.
- Use a unique, descriptive User-Agent in the form `<platform>:<app ID>:<version string> (by /u/<reddit username>)`. Default generic agents are "drastically limited". "NEVER lie about your user-agent."
- Prefer batched requests over per-item loops. `robots.txt` is for search engines, not API clients.

## Observed behavior (not policy)

- 2026-10-14, from this machine: `curl -A "reddit_kotw-research/0.1" "https://www.reddit.com/r/dominion/search.json?q=kotw&restrict_sr=1&limit=1"` returned `HTTP/1.1 403 Blocked` with a large HTML body (served by `snooserv`/varnish) instead of JSON. The same URL via a generic fetch tool also returned 403.
- This is one request from one network, not a documented policy. It does match the written requirement that access go through OAuth (Source A §2.8, Source C). `subreddit.R` builds `https://www.reddit.com/r/dominion/search.json?...` and per-post `.../comments/{id}` URLs without authentication or a custom User-Agent, so treat it as likely to fail intermittently or entirely.

## Gaps (could not be verified from a primary source)

- **Current numeric rate limit** and any free-tier terms. Reddit's help-center Data API Wiki (`support.reddithelp.com`) and the Responsible Builder Policy returned HTTP 403 to automated fetches. The 60 requests/minute figure is from the archived wiki only; re-check in a browser.
- **Whether a new app needs pre-approval.** The Terms require identification information and OAuth Access Info; the app-creation and approval process lives in the inaccessible Developer Documentation.
- **Exact attribution requirements** (Developer Documentation, not retrieved).
- **Whether a personal, non-commercial KotW archive counts as an "approved use case"**, and how that interacts with retaining raw historical records. This is a judgment to document, not something the source answers.
- I did not retrieve Reddit's Public Content Policy or User Agreement.

## Implications for this project

- A monthly job makes on the order of tens to low hundreds of requests, far under even the legacy 60 requests/minute figure. Rate limits are not the constraint; **access method and use-case scope are**.
- Move `subreddit.R` to OAuth with a descriptive User-Agent (a registered script app, credentials kept out of Git) and read the rate-limit headers.
- Keep raw posts and comments (the repo's evidence model wants them), but record in the repo that the retention purpose is a non-commercial recommender/analysis of r/dominion Kingdom posts. Anything retained beyond that scope is exposed under §3.2.
- The planned offline quarterly LLM analysis conflicts with §2.4/§3.2 if it is "training"; inference or classification over content is not explicitly addressed. Resolve before relying on it, and treat it as a decision for the owner.
- Plan a deletion/removal path: re-check source posts at each monthly collection and honor removals or author restrictions. The Terms do not prescribe the mechanism.
- Commercial use (e.g. a hosted product with revenue) needs a separate Reddit agreement (§3.1).

## Sources

- Reddit Data API Terms: https://redditinc.com/policies/data-api-terms
- Reddit Developer Terms: https://redditinc.com/policies/developer-terms
- Archived reddit API rules: https://github.com/reddit-archive/reddit/wiki/API
- Not reachable (403): https://support.reddithelp.com/hc/en-us/articles/16160319875092-Reddit-Data-API-Wiki
