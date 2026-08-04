# resident.assessment

Standalone resident/faculty evaluation app for IMSLU Internal Medicine.
Deployed on Posit Connect Cloud, public GitHub repo, secrets in env vars.

**Does not use `gmed`** — unlike most of the ecosystem, this app has its
own REDCap read/write via `httr::POST` (`RDM_TOKEN`, `FAC_TOKEN`). Keep it
that way unless there's a specific reason to add the gmed dependency;
it was built standalone on purpose.

## Architecture

`global.R` pulls resident + faculty + dictionary live on every startup —
no cache. Resident list: `data_functions.R::get_resident_data()` maps
REDCap label columns → `helpers.R::process_resident_data()` (drop
archived → `calculate_resident_level()` → filter available residents →
drop NA level).

7 assessment types live in `evaluation_helpers.R` (cc, obs, int_ip,
res_ip, bridge, cons, day). Availability is filtered by faculty division,
then by resident level. All types submit into one REDCap `assessment`
repeating instrument via `submit_evaluation_data()`. Resident level maps
to numeric `ass_level` (Intern=1, PGY2=2, PGY3=3, Rotator=1) at
`evaluation_form_builder.R:637`.

**Academic-year/cohort logic is fully date-driven off `Sys.Date()`**
(`helpers.R::calculate_resident_level`), rolling over at July 1 with no
hardcoded years anywhere in active code — no redeploy needed for the
rollover. `grad_yr` → `AcademicYear+1/+2/+3` maps to PGY3/PGY2/Intern,
`+4` maps to "Incoming" (hidden until July 1, then becomes "Intern").
Prelim/Rotator cohorts are 1-year, keyed off calendar year. This was
verified end-to-end against prod for the July 2026 rollover — re-run
`scratchpad/verify_july_readiness.R` (read-only, simulates dates) before
trusting the rollover again next year if the level logic changes.

## Gotchas

- **All rotators share `record_id` 157** — a single repeating record,
  special-cased in `data_functions.R`. Every rotator assessment submits
  under that one record id; don't assume `record_id` uniquely identifies
  a rotator.
- Graduated *categorical* residents fall through to NA level (not a
  "Graduated" status) and are dropped via the NA filter — they should
  ideally be archived in REDCap for cleanliness, but the app tolerates
  them un-archived.
- Per-assessment field lists are hand-maintained in matching
  `validate_*`/`collect_*` pairs in `evaluation_form_builder.R`, and must
  stay in sync with each `build_*_form()`'s field set — easy to drift
  silently (a past bug: Single Day Clinic's 3 milestone radios were
  displayed but never saved because `collect_day_form()` only handled
  `ass_cons_prof`).

## Adding a new assessment type

1. Fields must exist on the REDCap `assessment` form.
2. Register the type in `get_evaluation_types()`.
3. Add it to the relevant division list(s) in
   `get_available_eval_types_by_division()`, plus any level gating in
   `filter_eval_types_by_resident_level()`.
4. Write `build_*`/`validate_*`/`collect_*` — use
   `build_dict_assessment_form()` for simple radio-only forms.
5. Add a form-content switch case + submit observer in `server.R`.

Keep the three field lists (build/validate/collect) identical — that's
the #1 source of silent data loss in this app.

## Current status

On branch `claude/assessment-ui-updates` (not pushed — pushing triggers a
Connect deploy, which is Fred's call). Includes: SSM brand colors on
`bs_theme` in `ui.R`, faculty identity-confirmation modal, post-submit
routing choice modal, a "show all types" escape hatch bypassing
division/level filtering, and the Single Day Clinic milestone-save fix
above. Verify current branch/log before assuming this is still accurate.
