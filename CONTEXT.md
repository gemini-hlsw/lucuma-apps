# Explore — Observation Planning

Explore is the web application astronomers use to plan Gemini observations: stating what
they need from an observation and choosing an instrument configuration that satisfies it.

## Language

### Requirements and configuration

**Science Requirements**:
What an observation needs from the telescope, stated independently of any instrument —
wavelength, resolution, focal plane, exposure time mode. Persisted per observation.
_Avoid_: constraints (that is a separate concept: sky conditions)

**Observing Mode**:
The concrete instrument setup an observation will actually execute with.
_Avoid_: configuration (ambiguous — see Configuration)

**Configuration**:
A candidate instrument setup offered to the user, before it has been accepted onto the
observation. Becomes an Observing Mode only when accepted.

**Available Configurations**:
The table of Configurations matching the current Science Requirements. Its row set is
served by the ODB and narrowed by the Requirements, not chosen by hand.
_Avoid_: modes table, spectroscopy table

### Focal plane

**Focal Plane**:
The arrangement of apertures an instrument presents to the sky: Single Slit, Multiple
Slits, or IFU. A Science Requirement — stating one is a claim about what the observation
needs, and it doubles as the down-select for Available Configurations.
_Avoid_: FPU (see below), slit type

**FPU**:
Focal Plane Unit — the specific physical mask or aperture assembly on an instrument. An
FPU has a Focal Plane; the two are not interchangeable. Used as the Available
Configurations column heading.

**Focal Plane Angle**:
A minimum slit length an observation requires, in arcseconds. A separate Science
Requirement from Focal Plane, despite sharing the form row.

**MOS Mask**:
A fabricated plate carrying many slits, used by a Multiple Slits observation in place of a
builtin FPU. Uploaded as an attachment, and normally absent until well after the Observing
Mode is created — an unbound mask is the ordinary state, not an error.
_Avoid_: custom mask (that is the Observing Mode field holding the binding plus the slit
width), plate

**Mask Binding**:
The link from an Observing Mode to the MOS Mask its observation is taken through. It is
authoritative and part of the instrument's dynamic config, so it reaches every sequence
step — binding a mask is an instrument change, not bookkeeping. Distinct from tagging the
same attachment onto the observation, which is cosmetic and additive only.
_Avoid_: mask assignment, attachment link

**Custom Slit Width**:
The slit width of a MOS Mask, chosen from a per-instrument enum. Stated by hand while the
mask is unbound; once a mask is bound the plate defines it, so the stored value is shown as
read-only information rather than edited. It is the value last stated by hand, not read back
from the plate.

### Pointing

**Base Position**:
The point on the sky an observation is pointed at, and the origin every offset is measured
from. It always exists: unless overridden it is the centre of the science targets.
_Avoid_: centre, pointing origin

**Explicit Base**:
A Base Position stated by the user, overriding the computed one. Clearing it does not
remove the Base Position — it returns it to the computed value.
_Avoid_: custom base, manual base

**Slot**:
A named position an observation fills in its field. Instrument apertures are Slots (GHOST's
two IFU arms), and so is the Base Position. A Slot holds either a science target or a Sky
Position.
_Avoid_: arm, aperture (an aperture is a Slot, but the Base Position is not an aperture)

**Sky Position**:
A Slot deliberately pointed at blank sky instead of a target, so the observation samples
background through the same optics. Today only GHOST offers one.
_Avoid_: sky target, blank field

### Archive duplication

**Archive Duplication Search**:
The check the ODB runs against the Gemini Observatory Archive for one observation, asking what
the archive already holds around its pointing. Explore triggers it and reads its stored result;
nothing runs it automatically.
_Avoid_: query (that is one of the archive calls the ODB fans a single Search out into), archive
check

**Archive Match**:
One archived *file* a Search matched, described by the archive's own record of it. A single past
observation contributes several Archive Matches, and that is not deduplicated.
_Avoid_: duplicate, hit, result

**Search Area**:
The centre and radius a Search covered: coordinates for a sidereal pointing, a target name for a
non-sidereal one, with a radius derived from the observation's field of view. Stored with the
result, so it describes the pointing the result was gathered at rather than the observation as it
now stands.
_Avoid_: search region, field

**Match Count**:
How many Archive Matches a Search found, counted per file to stay consistent with the PIT. A
floor rather than an exact figure when the Search saturated the archive's 500-record cap.
_Avoid_: number of duplicates, hits

### Program scheduling

**Active Period**:
The date interval a program may be observed in, shown as Start and End on the Program Details
tile. A property of the program's award, not of anyone's decision to schedule it.
_Avoid_: active (bare — collides with Program Status Active)

**Program Status**:
Whether a program may be scheduled at all, and whether it is considered finished: Active,
Inactive, Complete or Incomplete. One value, not two flags — marking a program Complete
replaces Inactive rather than sitting alongside it. Complete does not mean the program used
all its allocated time. Readable by anyone with program access, settable only by staff.
_Avoid_: active/inactive (bare — say Program Status), proposal status (a separate concept:
where a proposal sits in the submission workflow)

### Tile layout

**Row Span**:
A tile's height in the grid, expressed in the layout's fixed row unit (`h` on the stored
layout item), not pixels.

**Stored Height**:
A Row Span persisted in user preferences from a past drag or the default layout. The
ordinary case: fixed until the user resizes the tile again.
_Avoid_: saved height, default height (a Stored Height starts as the default and is
overwritten by the user's own resizes)

**Auto-Height Tile**:
A tile whose Row Span is continuously derived from its content instead of a Stored Height.
Opted into per tile; cannot be resized vertically, since a manual resize would be
overwritten by the next measurement.

**Derived Height**:
The Row Span an Auto-Height Tile currently has, computed from its Natural Content Height.
Never comes from user preferences; a Stored Height for an Auto-Height Tile is dead data.

**Natural Content Height**:
How tall a tile's content and title bar actually are, unconstrained by the space the grid
has given the tile. What gets measured to compute a Derived Height.
_Avoid_: content height (ambiguous with the clipped, grid-constrained height)

# Telescope Offset Configuration

How Gemini instruments specify where the telescope points at each step of a
sequence, relative to a base position. The long-slit instruments (GNIRS, F2,
IGRINS-2) are being unified onto one shared model for this.

## Language

**Telescope Config**:
A single sequence step's pointing instruction: a spatial Offset plus a guide
state (whether the guider runs at that step).
_Avoid_: offset, position

**Spatial Offset**:
A telescope displacement in arcsec, with a tracking component (p) and a
slit-aligned component (q).
_Avoid_: position, coordinate

**Offset Shape** (offsetsType):
The structural form of a nod pattern — either *along the slit* (offsets stay on
the slit, q only) or *to the sky* (offsets leave the slit for blank sky, full
p/q). This is the discriminator carried by a slit telescope-config set.
_Avoid_: mode, type

**Nod**:
A deliberate telescope move between exposures of a science target, used to
sample background and cancel detector systematics.

**Nod Along Slit**:
A nod that stays within the slit aperture (q-only offsets). Guide stays on.

**Nod To Sky**:
A nod that leaves the slit to sample blank sky (large p/q offsets, guide off at
the sky positions).

**Preset**:
A named, ready-made nod pattern (a full set of telescope configs) offered as a
one-click starting point in the UI. A preset is **not stored** — selecting one
writes the resolved telescope configs; the chosen name is never round-tripped.
_Avoid_: template, mode

**Telluric**:
A preset (and a calibration target role) for observing a standard star to
correct atmospheric absorption. F2's default nod pattern.

### AEON / Multi-Facility Proposals

**AEON Multi-Facility Proposal**:
A Gemini proposal (Queue, Classical, or Large Program) whose project also requests
time at non-Gemini facilities. Membership is the presence of the proposal's
`aeonMultiFacility` object rather than a boolean, so the AEON Required Instruments
have nowhere to live unless the proposal is in the program. Explore models this as
`Option[AeonMultiFacility]`, so leaving the program and clearing the set are the
same edit.
_Avoid_: AEON proposal (AEON is the network, not the proposal), MF proposal,
multi-facility flag.

**AEON Required Instrument**:
An instrument an AEON Multi-Facility Proposal declares indispensable: the project is
infeasible without its requested Gemini time. Scheduling information only. Expressed
as a set — an instrument is required by being in the set, and every other instrument
is not required, which is the default. Site is never stated; it follows from the
instrument.
_Avoid_: required configuration ("configuration" means something else in the ODB),
required time flag.

**AEON Eligible Instrument**:
An instrument Explore will let a PI mark required: the program has a non-calibration
observation using it whose workflow state is Defined or higher. Stricter than the
ODB's own backing-observation rule, which accepts any active observation. See
`docs/adr/0008-aeon-required-instruments-track-defined-observations.md`.
_Avoid_: backing observation (that names the ODB's looser rule, not this one).
