# Research: Dominionforum access and terms

Ticket: `gstephan30/reddit_kotw#25`. Researched 2026-10-14 using a handful of single, polite fetches (descriptive User-Agent, ~3 s spacing, no crawling) of first-party pages only. Observed behavior is separated from written policy. Not legal advice.

## Short answer

- The forum is **`https://forum.dominion-welt.de`**, a **phpBB3** board titled "Dominion Forum" (German). `dominionforum.de` is not the real host.
- There is **no official export or JSON API**. A public, unauthenticated **Atom feed** exists for incremental recent posts; the archive is reachable only through paged HTML (`viewforum.php`, `viewtopic.php`).
- **Written policy is silent on automated access**: forum `robots.txt` returns 404, and the Terms of Use do not mention scraping. Silence is neither permission nor prohibition.
- **Recommendation: ask the operator (Cartamundi Deutschland GmbH) for permission before any archive backfill**, and use the feed (low load, intended for syndication) for incremental collection.

## Platform and structure (observed)

- The index page shows counters: 16,895 posts, 1,446 topics, 1,528 members (2026-10-14), so the full archive is small.
- Subforums on the index (`viewforum.php?f=N`): Allgemein (1), News (2), Feedback-Ecke (25), Strategiediskussionen (4), Regelfragen (5), eigene Kartensets (6), Fan-Karten (7), Tauschbörse (8), Dominion Online (30), then per-expansion forums (Basisspiel 10, Die Gilden 11, Die Intrige 12, Seaside 13, Mixbox 14, ...). Strategy and rules forums are the most relevant for Kingdom evidence.
- Pages are served to anonymous visitors without login and without a bot challenge. The index URL embeds a session id (`sid=`), which must be stripped when canonicalizing URLs.

## Export / API / feed

- No export or REST API was found on the site or in its legal pages.
- **Atom feed** `https://forum.dominion-welt.de/app.php/feed` (`application/atom+xml; charset=UTF-8`, `xml:lang="de"`) returned 15 `<entry>` items (latest posts across all forums) unauthenticated. This is standard phpBB feed behavior; per-forum/topic feed variants were not tested.
- Entry fields: `author/name`, `published`, `updated`, `id`/`link` (permalink `viewtopic.php?p=<post_id>#p<post_id>`), `title` (forum name prefix, "Re:" for replies), `category` (forum name and `viewforum.php?f=` URL), `content` (full HTML body with a footer linking the author profile `memberlist.php?mode=viewprofile&u=<user_id>`).
- The feed is a rolling window of 15 entries, so it can only be the incremental source if polled often (at least weekly given about 17k posts overall); the archive backfill must come from HTML.

## Fields available

| Field | Source | Notes |
|---|---|---|
| Post id | feed `id`, `viewtopic.php?p=` | stable permalink |
| Author name and user id | feed, profile link | |
| Date | feed `published`/`updated` | ISO 8601 with offset; `updated` differs when edited |
| Forum | feed `category`, `viewforum.php?f=` | |
| Thread title | feed `title` | topic id (`t=`) comes from HTML pages |
| Body | feed `content` (HTML) | German; keep original authoritative |
| Engagement | none in the feed | no vote/like field observed; HTML list views may show per-topic view/reply counts (not verified). Keep missing as unknown. |

## Robots, terms, and legal pages

- `https://forum.dominion-welt.de/robots.txt` returns **HTTP 404**: no directives, no crawl-delay.
- `https://www.dominion-welt.de/robots.txt` (marketing site) only disallows `/wp-admin/` (allows `admin-ajax.php`) and lists a sitemap; it does not cover the forum subdomain.
- Forum Terms of Use (`ucp.php?mode=terms`, German): access forms a usage contract between the visitor and the operator (§1). Posters grant the operator a simple, unlimited, royalty-free right to use posts *within the board* (§2). The operator may delete or lock accounts and posts and edit rule-violating posts (§3). §4 only notes phpBB is GPL v2 software. **Nothing addresses scraping, bulk archival or automated access, and nothing grants third parties reuse rights.**
- Operator (Impressum of www.dominion-welt.de, reported in the earlier source-access note): Cartamundi Deutschland GmbH, Altenburg. The marketing site's general copyright notice is not explicitly scoped to forum posts.

## Archive, then incremental

1. **Backfill** (needs operator OK): enumerate forums via `viewforum.php?f=N`, page topics, then page `viewtopic.php?t=<id>` and parse posts. Roughly 1.5k topics, so at most a few thousand requests, spaced 3-5 s, off-peak, with an identifying User-Agent and contact. Earliest Internet Archive snapshot is January 2017; the forum's own archive depth is unknown.
2. **Incremental**: poll the Atom feed, dedupe by post id, re-fetch HTML only for changed topics.
3. **Edits/deletions**: feed shows `updated`; deletions are not signalled. Keep original text with fetch timestamp per repo evidence rules.

## Should we ask the operator?

Yes. (a) No written automated-access policy exists, so permission cannot be assumed; (b) the Terms bind visitors, authors keep rights, and downstream reuse is not covered; (c) the operator is a single identifiable company and the forum is small, so asking is cheap; (d) usernames and posts are personal data under GDPR, which the retention plan must address. Suggested ask: one-time archive fetch at a stated rate, feed polling, private raw retention for non-public analysis, author attribution, honoring deletion requests. Until a reply, restrict to the feed and treat the HTML backfill as blocked. This unblocks `#28`.

## Gaps

- Per-forum/topic feed variants, topic list sorting, and per-topic view counts were not verified.
- Operator tolerance of bulk crawling is unknown; no contact address beyond the Impressum was checked.
- Absence of `robots.txt` was observed at a single point in time.

## Sources (first-party, fetched 2026-10-14)

- https://forum.dominion-welt.de/index.php
- https://forum.dominion-welt.de/app.php/feed
- https://forum.dominion-welt.de/robots.txt (404)
- https://forum.dominion-welt.de/ucp.php?mode=terms
- https://www.dominion-welt.de/robots.txt
