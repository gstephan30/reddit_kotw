# Research: source access and retention policy

Decision ticket: `gstephan30/reddit_kotw#2` ("Research source access and retention policy"), child of the Wayfinder map `#1`.

## Question

Which collection and retention methods are permitted and practically reliable for monthly ingestion of Reddit Kingdom-of-the-Week posts/comments, public BoardGameGeek (BGG) forum posts/replies, and Dominionforum's publicly accessible archive/feed? This note separates **verified written policy** (quoted/paraphrased from an official page, with URL) from **observed behavior** (what a live fetch actually returned, which is not itself a policy statement) for each source, then gives a cross-cutting summary for monthly-ingestion planning.

Research performed 2026-10-13 via direct fetches of official pages (and, where a live fetch was blocked, the closest available first-party mirror — noted explicitly below). No full source posts or rules text are reproduced here; quotes are limited to short policy phrases.

---

## 1. Reddit

Sources fetched directly: `redditinc.com/policies/data-api-terms` (Data API Terms) and `redditinc.com/policies/developer-terms` (Developer Terms).

### Verified policy
- **Registration is required**: accessing the Data APIs requires providing accurate, up-to-date "identification information (e.g., contact details)" — in practice, registering an OAuth/developer app (Data API Terms §1.3).
- **Access must be via the documented OAuth mechanism**, not scraping; you must not spoof the user agent or OAuth identity (§2.8).
- **Rate limits are reserved to Reddit's sole discretion** and are not numerically published in the Terms themselves (§2.9, §3.1); the exact QPM figure lives in Reddit's Developer Documentation, which could not be retrieved in this session (`support.reddithelp.com` and `reddit.com/wiki/api` both failed to load — see Gaps). Exceeding the limit requires a separate commercial agreement.
- **Retention is scoped, not time-boxed**: you may not "retain any User Content, Materials, or data accessed through the Data APIs beyond your approved use case," and must "immediately delete any data not required for it" (§3.2). On termination/revocation you must delete "any cached or stored User Content and Materials," including anything derived from it, such as model outputs (§6). There is no fixed day/week retention ceiling — the obligation is that ongoing retention must stay inside your declared use case.
- **No AI/ML training without separate rights**: the Terms grant no license "to use User Content for... training a machine learning or AI model" (§2.4).
- **Attribution is deferred to Developer Documentation** (§4.2: "display any attribution(s) required by Reddit as described in the Developer Documentation"); the Terms page itself only specifies the Reddit wordmark format ("[name] for Reddit") and does not spell out a per-post/per-author attribution rule.
- **Content ownership stays with the posting user**; Reddit's license to a developer is non-exclusive, non-transferable, revocable, and limited to copying/displaying content "solely as necessary" to run the app — any author-attached rights-reserved or Creative Commons marker must be honored (§2.4).
- **Deletion/removal requests**: Reddit may send notices/requests to remove content under the Reddit Rules, which must be escalated through Reddit's support channel (§2.5). The Terms do not spell out an explicit "must propagate live reddit.com deletions to your own archive" rule, but this is implied by the scoped, revocable license in §2.4 and the retention limit in §3.2.
- Developers must also comply with the Reddit User Agreement and Privacy Policy, and must disclose their own data handling to end users (§2.3, §2.6).

### Observed behavior
- None beyond confirming the policy pages load live; no live API probing was performed in this research pass.

### Gaps / uncertainties
- The numeric OAuth rate limit for a script-type app is not confirmed from an official page in this session (attempts to reach the Data API Wiki articles failed); treat any previously-known figure (e.g., ~60 requests/minute) as unverified until re-checked against `support.reddithelp.com`'s current Data API Wiki.
- The exact attribution format (e.g., whether a permalink/username must be shown per item) is not specified in the Terms and requires the Developer Documentation, which was not accessible in this session.
- No bright-line retention-days limit exists; "not required for your approved use case" is a judgment call that should be documented against this project's stated purpose (monthly archival for a personal Dominion recommender) if ever reviewed by Reddit.

---

## 2. BoardGameGeek (BGG XML API)

Live fetches to `boardgamegeek.com` were blocked by a Cloudflare bot challenge in this session; the citations below are from Wayback Machine snapshots of the same official URLs (dated within the last several months), the closest first-party source reachable. A live, browser-based re-check is recommended before relying on the still-unresolved rate-limit point.

Sources: `boardgamegeek.com/wiki/page/XML_API_Terms_of_Use`, `boardgamegeek.com/using_the_xml_api`, `boardgamegeek.com/terms`, `boardgamegeek.com/wiki/page/BGG_XML_API2`.

### Verified policy
- **Non-commercial license scope**: BGG grants a "worldwide, non-exclusive, royalty-free license to reproduce and display the data... solely for strictly non-commercial purposes" — matches a personal hobby project.
- **AI/LLM training is explicitly prohibited**, both at the XML API level and the general site ToS level ("Use of the XML API—or any of the data on the site—to train an AI... or Large Language Model (LLM) system is strictly prohibited"). This directly constrains this project's planned offline LLM-assisted evidence batch if it ever ingests BGG-sourced text into that batch.
- **No modification of retrieved data** is permitted.
- **Attribution is mandatory**: "credit BoardGameGeek by name as the source of the data," and public-facing uses must show a linked "Powered by BGG" logo.
- **Registration/authorization is required** for programmatic XML API use via `boardgamegeek.com/applications`; approval can take a week or more. Narrow exceptions exist for a logged-in user pulling their own collection (no registration) or another user's collection while logged in (registration-free but "heavily rate limited").
- **No fixed numeric rate limit is published**: BGG states it is "still determining exact usage limits for the XML API," and instead gives qualitative guidance — cache responses server-side, keep request counts to a minimum, avoid direct client-side calls, and note that usage limits may differ by license status.
- **Anti-scraping clause** in the general ToS (§6.B.i): automated systems must not send "more request messages... than a human can reasonably produce in the same period," except as expressly permitted.
- **No PII harvesting** (§6.B.ii): no collecting of account names or other personal data beyond what's needed.
- **Content removal**: BGG "reserves the right to remove or delete User Submissions without prior notice," and separately states it "may keep deleted User Submissions" itself — i.e., BGG can change or remove content you've already fetched with no notice to downstream consumers, and BGG's own retention of "deleted" content may outlast the user-visible deletion.
- **DMCA takedown process** is defined (§11) with a named copyright agent, relevant if a rights holder later objects to retained BGG forum text.
- **`robots.txt` (fetched live)**: `Crawl-delay: 5`; explicit `Disallow: /xmlapi` for the generic crawler block (separate from the registered-application XML API access regime, which is governed by its own Terms of Use rather than robots.txt); an extensive list of AI/LLM crawler user agents (GPTBot, ClaudeBot, CCBot, Google-Extended, PerplexityBot, etc.) is blocked outright — consistent with the written "no AI/LLM training" clause.

### Observed behavior
- Direct automated fetches to `boardgamegeek.com` (wiki pages, `/terms`) returned HTTP 403/401 behind a Cloudflare JavaScript challenge in this session — an operational access friction, not a written policy. The XML API itself (bearer-token authenticated) was not probed live in this pass.

### Gaps / uncertainties
- All BGG policy citations above come from Wayback Machine mirrors, not a live fetch in this session, because of the Cloudflare challenge; re-verify against the live pages (ideally with a real browser or a cooperating client) before final reliance, especially for anything time-sensitive.
- No documented numeric rate ceiling exists at all; a monthly batch job has no official number to engineer against and should self-throttle conservatively (e.g., low request rate, caching) and expect to observe 429s/ throttling empirically rather than from a published limit.
- Whether "non-commercial purposes only" retention implies any maximum retention duration is unstated — unlike Reddit, there is no "delete data not required for your use case" style clause specific to the API Terms (the broader "do not modify" and "non-commercial" framing constrain use, not duration).

---

## 3. Dominionforum (`forum.dominion-welt.de`)

The commonly assumed domain `dominionforum.de` does not exist (no DNS resolution, no Wayback Machine CDX records for that hostname). The actual community forum, confirmed via a citation in the German Wikipedia article on Dominion, is **`forum.dominion-welt.de`**, a phpBB3 board operated by **Cartamundi Deutschland GmbH** (German Dominion publisher, trading as ASS Altenburger), alongside the WordPress marketing site `www.dominion-welt.de`.

Sources fetched directly: `www.dominion-welt.de/impressum/`, `www.dominion-welt.de/datenschutzerklaerung/`, `forum.dominion-welt.de/ucp.php?mode=terms`, `forum.dominion-welt.de/ucp.php?mode=privacy`, `www.dominion-welt.de/robots.txt`, `forum.dominion-welt.de/robots.txt`.

### Verified policy
- **Operator identity** (Impressum): Cartamundi Deutschland GmbH, Altenburg, Handelsregister Jena HRB 502240. A general copyright notice on the marketing site prohibits unauthorized reproduction/processing/distribution of its texts/images/graphics; this is phrased for the marketing site and does not explicitly name the forum, so its application to forum posts is ambiguous rather than confirmed.
- **Site privacy policy** (marketing site Datenschutzerklärung): server log files (IP, timestamp, requested page, status, referrer, browser/OS) are retained with IP anonymized before storage, and log data is automatically deleted after 7 days with limited exceptions. This covers the marketing site's own server logs, not third-party dataset retention of forum content.
- **Forum Terms of Use** (`ucp.php?mode=terms`): posting grants the forum operator a simple, unlimited-in-time-and-place, royalty-free right to use a member's post within the board, surviving account termination. The operator may delete or lock any account, post, or function at any time, and may edit posts that violate rules. These Terms govern the operator↔registered-poster relationship; **no clause anywhere addresses third-party scraping, archiving, or API/automated access** — unlike Reddit or BGG, there is no developer-terms-style regime at all.
- **Forum privacy policy** (`ucp.php?mode=privacy`): describes session cookies (1-year default), registration data, and that posts/private messages store the poster's IP at creation/edit/deletion/failed-login time. It explicitly acknowledges that "the data of your profile and the posts you create may be publicly accessible on the Internet" — an acknowledgment of public visibility, not a third-party reuse grant.
- **`robots.txt`**: the marketing site's robots.txt (live, direct fetch) disallows only `/wp-admin/` and lists a sitemap — no crawl-delay, no forum-specific rule. The forum subdomain's own `robots.txt` (live, direct fetch) **returns HTTP 404 — no file exists**, which is a verified absence-of-directive fact, not an explicit permission or prohibition either way.
- **No written statement anywhere found** (Impressum, Datenschutzerklärung, forum Terms of Use, or forum privacy policy) that addresses scraping, bulk/automated archival, or third-party API access — this is the weakest-governed of the three sources in terms of explicit written policy.

### Observed behavior (not written policy)
- **Public read access with no login wall**: unauthenticated fetches of the forum homepage, an individual thread (`viewtopic.php`), and a keyword search (`search.php`) all succeeded and returned real content (usernames, post text, timestamps); the homepage's visible guest counter corroborates this is the forum's intended behavior for anonymous visitors, though it is inferred from successful responses rather than a stated "guests may read" policy.
- **A public, unauthenticated Atom feed is live**: `https://forum.dominion-welt.de/app.php/feed` returns valid `application/atom+xml` with recent posts across forums, full HTML content, author, subforum, and timestamp per entry (standard phpBB "recent posts" feed behavior; `feed.php` 301-redirects to the same endpoint). This is directly observed, not documented in any ToS/privacy page found.
- **Activity at time of fetch**: roughly 16,900 posts, 1,450 topics, 1,530 members, with the most recent feed entry dated the fetch day — an actively maintained, moderate-size community.
- **Approximate archive depth (via Internet Archive, not the forum's own records)**: Wayback Machine CDX snapshots of `forum.dominion-welt.de` go back to January 2017, with no earlier snapshots found in the ranges queried; this reflects Internet Archive's crawl history, not necessarily the forum's true founding date or its own oldest live post (a clean oldest-first sort view did not return usable results in this session).
- **No Cloudflare-style bot challenge observed** (unlike BGG) — direct automated fetches succeeded without a JS challenge.

### Gaps / uncertainties
- No explicit crawling/archiving permission or prohibition exists in writing for third parties; the only loosely relevant clause is the ambiguous marketing-site copyright notice, which does not clearly extend to forum content.
- No REST/JSON API exists beyond the standard Atom "recent posts" feed; broader ingestion would mean paging through `viewtopic.php`/`viewforum.php`/`search.php` HTML, which is neither authorized nor prohibited in writing for non-interactive clients.
- No deletion/edit-propagation obligation is stated for third parties; the forum's own Terms only address the operator's right to delete/alter member posts, not a downstream archive's obligation to mirror such changes.
- The forum's true oldest post/founding date could not be confirmed beyond the 2017 earliest Wayback snapshot.
- The persistence (or temporary nature) of the forum subdomain's missing `robots.txt` was only confirmed at a single point in time in this session.

---

## Cross-cutting summary

| Dimension | Reddit | BGG (XML API) | Dominionforum (`forum.dominion-welt.de`) |
|---|---|---|---|
| Monthly ingestion feasibility | Permitted via OAuth within a declared, approved use case; no scraping | Permitted for non-commercial use once an application is registered and approved (approval can take a week+); no numeric ceiling published, so self-throttle | No formal API/ToS regime either way for third parties; only unauthenticated HTML/Atom-feed access, with no documented authorization or prohibition — weakest-governed of the three |
| Text retention | No fixed day limit; retention must stay within the "approved use case," with mandatory deletion on termination/revocation | No stated retention limit for non-commercial licensees; BGG itself may alter/delete source content without notice and may still retain "deleted" content server-side | No stated retention limit for third parties at all; the forum's own privacy policy only covers its 7-day server-log retention, unrelated to a downstream archive |
| Attribution | Required "as described in Developer Documentation" (not retrieved); wordmark format constrained to "[name] for Reddit" | Required: credit "BoardGameGeek" by name plus a linked "Powered by BGG" logo on public-facing use | No attribution requirement documented anywhere |
| Deletion/takedown handling | Must escalate Reddit-issued removal notices via its support form; must honor author-attached rights-reserved/CC markers | BGG may delete/alter content without notice; DMCA process with a named copyright agent exists | No documented third-party takedown channel; only in-forum moderation by the operator is addressed |
| API/feed rate or access limits | Reserved to Reddit's sole discretion; no number published in the Terms; exceeding limits needs a separate agreement | Explicitly "still determining" — qualitative guidance only (cache, minimize requests, avoid direct client calls) | No `robots.txt` exists for the forum subdomain (404); no crawl-delay or rate guidance published anywhere for the forum |
| Access/registration requirement | OAuth app registration mandatory, with accurate identification info | XML API application registration mandatory for nearly all programmatic use (narrow logged-in exceptions) | No registration needed to read posts, search, or consume the Atom feed; registration only needed to post as a member |

## Methodology notes

- Reddit's policy pages were reached via a direct live fetch; BGG's were reached only via Wayback Machine mirrors of the same official URLs after the live site returned a Cloudflare bot challenge (403/401) to every direct automated fetch attempt in this session — a live, browser-based spot-check is recommended before final reliance, particularly for BGG's still-undetermined rate limits. Dominionforum pages were reached via direct live fetch with no blocking observed.
- The assumed domain `dominionforum.de` does not exist; the real forum (`forum.dominion-welt.de`) was located via a citation trail through the German Wikipedia article on Dominion rather than search engines, which were blocked or unproductive in this session.
- No full source posts, forum threads, or copyrighted rules text are reproduced in this report; all quotations above are short policy phrases from official Terms/Privacy pages, cited by URL.
