# Research: Official card and rules coverage vs. checked-in `dominiontabs` catalog

- **Ticket:** gstephan30/reddit_kotw#3 ("Research official card and rules coverage"), child of #1 (Wayfinder map)
- **Scope:** Compare official Dominion expansion/edition/rules status against this repo's checked-in `sumpfork/dominiontabs` data in `cards/`. Report catalog identifier/schema, completeness/gaps, newest official-set status, rules-source dates/versions, and uncertainties. No copyrighted rulebook text is reproduced — citations and short paraphrases only.
- **Evidence gathered:** 2026-10-13 (see individual citation access context below; most primary sources could not be timestamp-confirmed beyond "fetched this session" because they don't expose a page-level revision date).

## 1. Stable catalog identifier / schema (as checked in)

The repo's `cards/README.md` points to `https://github.com/sumpfork/dominiontabs` as the data source (`reddit_kotw:cards/README.md:3`), and `.github/copilot-instructions.md` designates the curated `card_tag` field as the cross-language Card identity (`reddit_kotw:.github/copilot-instructions.md:35`, uncommitted working file, not yet in git history).

Checked-in files (`reddit_kotw:cards/`):
- `cards_db.json` — 570 card records, each with `card_tag` (stable id), `cardset_tags` (list of lowercase set keys), `cost`, `group_tag`, `randomizer`, `types`. Example: `{'card_tag': 'Alms', 'cardset_tags': ['adventures'], 'cost': '0', 'group_tag': 'adventures events', 'randomizer': False, 'types': ['Event']}`.
- `sets_db.json` — 22 set keys (metadata only, no card lists): `adventures, alchemy, animals, base, cornucopia, dark ages, dominion1stEdition, dominion2ndEdition, dominion2ndEditionUpgrade, empires, extras, guilds, hinterlands, intrigue1stEdition, intrigue2ndEdition, intrigue2ndEditionUpgrade, menagerie, nocturne, promo, prosperity, renaissance, seaside`.
- `expansions.csv` — 17 rows of `expansion,release` pairs (e.g. `Dominion1stEdition,2008-10-01`; newest row `Menagerie,2020-03-01`).
- `labels_db.json`, `types_db.json`, per-language folders (`cz`, `de`, `en_us`, `fr`, `it`, `nl_du`, `xx`), `translation.md`.

**Provenance of the checked-in snapshot:** `git log --follow -- cards/sets_db.json` shows the only commit touching this data is `a57ffbeca0` ("started improved card detection"), dated **2021-02-13** (`reddit_kotw` git history). There has been no update since; the snapshot is roughly **4.7 years stale** relative to upstream as of this research.

## 2. Comparison with current upstream `sumpfork/dominiontabs`

Upstream repo metadata (GitHub API, `sumpfork/dominiontabs`, fetched this session): `pushed_at: 2026-10-13T04:01:39Z`, actively maintained (151 stars, 42 open issues). Current source-of-truth paths moved to `card_db_src/` (upstream commit `316db5f684d1db287aaae931396500741ee34bd5`): `sumpfork/dominiontabs:card_db_src/sets_db.json`, `sumpfork/dominiontabs:card_db_src/cards_db.json`.

Upstream `card_db_src/sets_db.json` (fetched raw, this commit) contains **35 set keys**, including 1st/2nd-edition/upgrade/removed variants not present locally, e.g.: `allies`, `plunder`, `risingSun`, `cornucopia1stEdition`, `cornucopia1stEditionRemoved`, `cornucopia2ndEditionUpgrade`, `cornucopiaAndGuilds2ndEdition`, `darkAges` (local key is the differently-spelled `dark ages`), `dominion1stEditionRemoved`, `guilds1stEdition`/`guilds1stEditionRemoved`/`guilds2ndEditionUpgrade`, `hinterlands1stEdition`/`...Removed`/`hinterlands2ndEdition`/`...Upgrade`, `intrigue1stEditionRemoved`, `prosperity1stEdition`/`...Removed`/`prosperity2ndEdition`/`...Upgrade`, `seaside1stEdition`/`...Removed`/`seaside2ndEdition`/`...Upgrade`, `promo-bigbox2-de`, `guilds-bigbox2-de`.

**Gaps in the checked-in catalog relative to current upstream:**
- Missing expansions entirely: **Allies**, **Plunder**, **Rising Sun** (all released after the Feb-2021 snapshot; see §3).
- Missing 2nd-edition/upgrade/removed set variants for Seaside, Prosperity, Hinterlands, Cornucopia, Guilds (upstream models these as distinct `*1stEdition`, `*2ndEdition`, `*Upgrade`, `*Removed` keys with `edition` and `upgrades`/`removed` linkage fields, e.g. `sumpfork/dominiontabs:card_db_src/sets_db.json` entry for `seaside2ndEditionUpgrade` has `"upgrades": "seaside1stEdition"`). The local `sets_db.json` instead has flat single keys (`seaside`, `prosperity`, `hinterlands`, `cornucopia`, `guilds`) with no 1st/2nd-edition distinction.
- Local `dominion2ndEditionUpgrade`/`intrigue2ndEditionUpgrade` exist but without the matching `*Removed` companion keys upstream now carries, and without the `allies`/`plunder`/`risingSun` card lists in `cards_db.json`.
- README confirms the upstream CLI already exposes `--expansions=seaside2ndEdition` (`sumpfork/dominiontabs:README.md`, Docker usage example), which the local snapshot's flat `seaside` key cannot address with edition specificity.
- Local `cards_db.json` is 570 cards across the 22 sets above (counts by `cardset_tags`, verified by direct tally): adventures 58, alchemy 13, animals 3, base 12, cornucopia 18, dark ages 42, dominion1stEdition 34, dominion2ndEdition 34, dominion2ndEditionUpgrade 7, empires 63, guilds 13, hinterlands 26, intrigue1stEdition 34, intrigue2ndEdition 26, intrigue2ndEditionUpgrade 7, menagerie 71, nocturne 75, promo 12, prosperity 27, renaissance 50, seaside 26 (no `risingSun`/`allies`/`plunder` rows at all).
- Package/version evidence: PyPI `domdiv` (the installable form of this same catalog) is at version **4.9.3** (PyPI JSON API, `https://pypi.org/pypi/domdiv/json`, fetched this session), vs. the locally vendored copy frozen at the 2021-02-13 commit — i.e. many releases behind.

**Not a gap, likely intentional:** the local catalog already includes the fan-made `animals` set (3 cards), which upstream also marks `"fan": true` — this is consistent, not evidence of drift.

## 3. Newest official-set status (as of this evidence)

Per Wikipedia's "Dominion (card game)" article (`en.wikipedia.org/wiki/Dominion_(card_game)`, raw wikitext fetched this session, itself citing Rio Grande Games product pages — Wikipedia is a secondary source used here only to assemble/date-check the primary citations, each individually cited below):

- **Rising Sun** — latest **released** official expansion, dated **September 2024**, cited to `https://www.riograndegames.com/games/dominion-rising-sun/` (Wikipedia ref `rising-sun`, access-date 14 August 2024). Paraphrase only: introduces "Shadow" cards playable from the deck and "Prophecies" (delayed rule changes), feudal-Japan theme.
- **Arcana** — **announced, unreleased**. Per Wikipedia, "In December 2025, the name and box art were revealed on Facebook," citing `https://www.facebook.com/100047948269439/posts/1564293351845592/` (Rio Grande Games Facebook page, access-date 18 March 2026). Wikipedia's own expansion table lists Arcana's release date as "2026 (predicted)" with cards/piles unknown ("?"). **This is the only known announced-but-unreleased official set as of this research; its scope, cards, and rules text are not yet public.**
- Prior to Rising Sun, the released-expansion order (with dates, from the same Wikipedia table, each row itself citing a Rio Grande Games product page) is: Dominion (Oct 2008 / 2nd ed Oct 2016), Intrigue (Jul 2009 / 2nd ed Oct 2016), Seaside (Oct 2009 / 2nd ed Jun 2022), Alchemy (May 2010), Prosperity (Oct 2010 / 2nd ed Jun 2022), Cornucopia (Jun 2011), Hinterlands (Oct 2011 / 2nd ed Jul 2022), Dark Ages (Aug 2012), Guilds (Jun 2013), Adventures (Apr 2015), Empires (Jun 2016), Nocturne (Nov 2017), Renaissance (Nov 2018), Menagerie (Mar 2020), Allies (Mar 2022), Plunder (Dec 2022), Rising Sun (Sep 2024). Cornucopia & Guilds also received a combined "Update Pack" dated March 2024 (2nd-edition reprint), matching upstream's `cornucopiaAndGuilds2ndEdition` set key.
- Wikipedia's own page banner states "sixteen expansions... released" as of 3 October 2026 — consistent with the 16 released sets enumerated above (Arcana not counted, as unreleased).

**Caveat:** I could not independently load the individual Rio Grande Games product pages or the official rulebook PDF directly — `riograndegames.com` returned HTTP 403 to automated fetches in this session (tried `riograndegames.com/our-games/dominion/` and the `Dominion2nd.pdf` rulebook URL cited by Wikipedia: `https://www.riograndegames.com/wp-content/uploads/2016/09/Dominion2nd.pdf`). The Rising Sun/Arcana facts above rely on Wikipedia's sourced citations to those same pages rather than my own direct read of the publisher page; treat the release-date/announcement facts as well-sourced but not independently re-verified against the live publisher page in this session.

## 4. Official rules references: dates/versions found

- **Dominion 2nd Edition Rulebook** (base game) — cited URL `https://www.riograndegames.com/wp-content/uploads/2016/09/Dominion2nd.pdf` (Wikipedia citation, access-date 19 January 2023). Could not fetch directly in this session (403); no in-document version/date could be verified beyond the implied 2016 2nd-edition timeframe from the URL path and the "October 2016" 2nd-edition release date in the Wikipedia table.
- No other dated official rules/errata documents (e.g., per-expansion rulebook revisions, FAQ, or designer errata pages) were located or verified in this session. The repo's own instructions (`.github/copilot-instructions.md`, uncommitted working file) already flag that "official rules/errata and explicit designer rulings... are normative only after human verification" and that rule PDFs should stay outside Git — no such rules-reference index currently exists in the repository.
- Donald X. Vaccarino's BGG design-notes posts (mentioned as a candidate source in the repo's domain rules) were not investigated in this session — BoardGameGeek pages and its XML API both returned HTTP 403/401 to automated fetches here, so no BGG content (rulings, expansion pages, forum posts) could be directly verified; this is a concrete gap for follow-up with authenticated/human access.

## 5. Explicit uncertainties and gaps

1. **Rulebook currency unverified directly.** The specific wording/version/date inside the Dominion 2nd Edition rulebook PDF, and whether later 2nd-edition expansions (Seaside/Prosperity/Hinterlands/Cornucopia&Guilds, 2022–2024) shipped updated rulebooks with dated errata, were not confirmed — publisher site blocked automated fetch (403) in this session.
2. **BGG not reachable.** Both `boardgamegeek.com` HTML and its `xmlapi2` endpoint returned 403/401 here, so no official BGG game/expansion page, designer post, or errata thread could be cross-checked directly; all Rising Sun/Arcana facts trace through Wikipedia's citations to Rio Grande Games pages only.
3. **Arcana is unreleased and unspecified.** No card list, rules text, or firm release date exists publicly as of this research (Wikipedia table: "2026 (predicted)", cards/piles "?"). The eligible catalog cannot include it until official materials are published.
4. **Exact card-level delta for Allies/Plunder/Rising Sun not enumerated.** This research confirms these three expansions are *entirely absent* from the checked-in `cards_db.json`/`sets_db.json` (no matching `cardset_tags`/set keys), but did not enumerate their full card lists card-by-card, since the task specifically excludes reproducing rulebook/card text; the presence/absence check was done by key lookup, not text diffing.
5. **2nd-edition modeling gap is structural, not just missing data.** Even where a set's cards look numerically similar (e.g., local `seaside` 26 cards vs. upstream's split `seaside1stEdition`/`seaside2ndEdition`/`seaside2ndEditionUpgrade`), the local schema cannot distinguish 1st vs. 2nd edition identity or edition-only reprints, which matters for the "preserve historical edition/rules context" rule already recorded in this repo's working instructions.
6. **No committed rules-reference index exists yet in-repo.** The repository currently has no versioned list of official rules-source dates (as called for by the parent issue's "Not yet specified" list, #1); this report is research input, not that index.
7. **Upstream `cards_db.json` full current contents not diffed line-by-line.** I confirmed upstream set-key coverage and the README's edition-aware CLI example, but did not download/diff the full current `card_db_src/cards_db.json` (large file) against the local 570-row file; a follow-up could script an exact structural diff if/when the catalog is actually updated.

## Citations

- `reddit_kotw:cards/README.md` — data-source pointer to `sumpfork/dominiontabs`.
- `reddit_kotw:cards/expansions.csv` — local expansion/release-date rows (newest: Menagerie 2020-03-01).
- `reddit_kotw:cards/sets_db.json` — 22 local set keys.
- `reddit_kotw:cards/cards_db.json` — 570 local card rows, `card_tag`/`cardset_tags` schema.
- `reddit_kotw` git history: commit `a57ffbeca05e4fdb2a0f29a2c3f604f659380b3b` (2021-02-13) — last change to `cards/`.
- `sumpfork/dominiontabs:README.md` (commit `316db5f684d1db287aaae931396500741ee34bd5`) — current install/usage docs, `seaside2ndEdition` CLI example.
- `sumpfork/dominiontabs:card_db_src/sets_db.json` (commit `316db5f684d1db287aaae931396500741ee34bd5`) — current 35 upstream set keys.
- `https://api.github.com/repos/sumpfork/dominiontabs` — repo metadata, `pushed_at: 2026-10-13T04:01:39Z`.
- `https://pypi.org/pypi/domdiv/json` — current PyPI package version `4.9.3`.
- `https://en.wikipedia.org/wiki/Dominion_(card_game)` (raw wikitext, fetched this session) — expansion release-date table and citations to Rio Grande Games product pages, including:
  - `https://www.riograndegames.com/games/dominion-rising-sun/` (Rising Sun, access-date 14 Aug 2024)
  - `https://www.facebook.com/100047948269439/posts/1564293351845592/` (Arcana name/box-art reveal, access-date 18 Mar 2026)
  - `https://www.riograndegames.com/games/dominion-allies/` (Allies, access-date 19 Jan 2023)
  - `https://www.riograndegames.com/games/dominion-plunder/` (Plunder, access-date 19 Jan 2023)
  - `https://www.riograndegames.com/wp-content/uploads/2016/09/Dominion2nd.pdf` (2nd Edition Rulebook, access-date 19 Jan 2023; not independently re-fetched, returned HTTP 403 in this session)
- Attempted but blocked in this session (HTTP 403/401, noted as gaps): `riograndegames.com` pages directly, `boardgamegeek.com` HTML and `xmlapi2`.
