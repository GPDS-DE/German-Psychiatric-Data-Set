# Core elements of a German psychiatric patient record in FHIR
Is it possible to identify and integrate core elements of a psychiatric patient record, and can they be fully coded in an internationally standardized way, stored in the central IT infrastructure of a clinic (in Germany) independently of the manufacturer and automatically contextualized for evaluation? 

The aim of standardization in FHIR® is interoperable data processing of health information. This includes biological measurement data, but more frequently the collection of health information using questionnaires.

The data that is relevant for processing the research question is compiled here.

# Project Overview
This project outlines the FHIR specification of the German
Psychiatric Data Set (GPDS). The FHIR instances, profiles, and implementation guides developed in this context serve as a specification for the syntactic and semantic interoperabilty of psychiatric instruments in Germany.

# Status
Current Stable Version: 1.0.0

All FHIR artifacts published within this project are assigned a status that indicates their maturity level. Profiles marked as 'Draft' have not yet been balloted and are subject to significant changes.

Opportunities for Contribution and Feedback
Submission of Comments: via GitHub

Discussion Forum: international FHIR chat

Participation: Interoperability Forum

# Summary
This Minimal Data Set (MDS) for psychiatric instruments includes PROMs, CROMs and Psychopathological Assessments. The MDS primarily covers the fields of psychiatry and questionnaire data.

# Content of this repository
**FHIR artifacts (JSON):** the profile `StructureDefinition-psych-befund-questionnaire.json` (PsychQuestionnaire, derived from the SDC Base Questionnaire, HL7 SDC 3.0.0), the Questionnaire instances of instruments that may be published, the ValueSets, example instances and `ImplementationGuide-psycho.json`. Instruments with licensing restrictions are provided as ValueSets only.

**Mapping tables:**
- `Mapping_of_SNOMED_CT_ICF_and_ICD_to_PROMs_CROMs_07102026.xlsx`: current version (release 1.0.0). The first sheet lists all changes compared with the previous version.
- `Mapping of SNOMED CT, ICF and ICD to PROMs & CROMs_30012026.xlsx`: previous version, kept for transparency. Do not use for implementation.
- `Kodierung von psychiatrischen Befunden_AMDP_CASCAP 2_070726.xlsx`: ICD-10-GM mappings for AMDP and CASCAP-2.
- The files per instrument and terminology (e.g. `HAMD-7 SNOMED CT.xlsx`) and `ISO-*.xlsx` are the inputs of the R scripts.

**Analysis scripts:** `R Analysen Interrater Agreement.R` (Krippendorff's alpha and proportion of agreement) and `Plot Vorbereitung_*.R` (equivalence measures according to ISO/TS 21564). Run them from the repository folder.

# License
Own artifacts (profile, ValueSets, mappings, example instances, scripts and documentation) are licensed under CC BY 4.0 (see `LICENSE`).

Third-party content is not covered by this license and remains subject to its own terms: SNOMED CT (SNOMED International; use requires an affiliate or national member licence), LOINC (Regenstrief Institute), ICD-10-GM (BfArM/WHO), ICD-11 and ICF (WHO), NCI Thesaurus (U.S. National Cancer Institute), and the wording, answer options and scoring rules of the psychiatric instruments (respective rights holders).

# Authors and Contacts 
# Consulted Experts:
Sylvia Thun (BIH@Charité)

Christoph Correll (Charité)

Malek Bajbouj (Charité)

Julian Hellmann-Regen (Charité)

Sabine Aust (Charité)

Viktor Nöhles (Charité)

Piper Ranallo (University of Minnesota)

William Bosl (University of San Francisco)

# Technical Implementation:
Nina Haffer (Charité)

Julian Saß (BIH@Charité)

Paul Müller (BIH@Charité)

Lotte Schwiening (BIH@Charité)

# Contact Information:
nina.haffer@charite.de
