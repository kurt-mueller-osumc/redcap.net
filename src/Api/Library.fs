namespace Api

module Common =
    type TccId = TccId of string
    type AvatarId = AvatarId of string
    type MRN = MRN of int

module Gender =
    type Gender =
        private
        | Male
        | Female
        | ``Other (intersex, disorders of sexual development previously classified as hermaphrodite)``
        | ``Transsexual, NOS``
        | ``Transsexual, natal male``
        | ``Transsexual, natal female``
        | ``Not stated or Unknown``

    let fromCode code =
        match code with
        | "01" -> Ok Male
        | "02" -> Ok Female
        | "03" -> Ok ``Other (intersex, disorders of sexual development previously classified as hermaphrodite)``
        | "04" -> Ok ``Transsexual, NOS``
        | "05" -> Ok ``Transsexual, natal male``
        | "06" -> Ok ``Transsexual, natal female``
        | "09" -> Ok ``Not stated or Unknown``
        | _ -> Error ("Gender code not found: " + code)

    let toCode gender =
        match gender with
        | Male -> "01"
        | Female -> "02"
        | ``Other (intersex, disorders of sexual development previously classified as hermaphrodite)`` -> "03"
        | ``Transsexual, NOS`` -> "04"
        | ``Transsexual, natal male`` -> "05"
        | ``Transsexual, natal female`` -> "06"
        | ``Not stated or Unknown`` -> "09"

module Race =
    type Race =
        private
        | White
        | Black
        | ``American Indian, Aleutian, or Eskimo (includes all indigenous populations of the Western Hemisphere)``
        | Chinese
        | Japanese
        | Filipino
        | Hawaiian
        | Korean
        | Vietnamese
        | Laotian
        | Hmong
        | ``Kampuchean (Cambodian)``
        | Thai
        | ``Asian Indian or Pakistani, NOS (code 09 prior to Version 12)``
        | ``Asian Indian``
        | Pakistani
        | ``Micronesian, NOS``
        | ``Chamorro or Chamoru``
        | ``Guamanian, NOS``
        | ``Polynesian, NOS``
        | Tahitian
        | Samoan
        | Tongan
        | ``Melanesian, NOS``
        | ``Fiji Islander``
        | ``New Guinean``
        | ``Other Asian, including Asian, NOS and Oriental, NOS``
        | ``Pacific Islander, NOS``
        | Other
        | Unknown
        | ``REFUSED TO ANSWER``
        | ``PT NOT AVAILABLE``
        | SOMALI
        | ``ASIAN CAMBODIAN``
        | NEPALI
        | ``AFRICAN OTHER``
        | ``NTV HAWAIIAN or PCF ISL``
        | ``MIDDLE EASTERN NORTHERN AFRICAN``
        | ``ASIAN LAOTIAN``
        | ``MORE THAN ONE RACE``

    let fromCode code =
        match code with
        | "01" -> Ok White
        | "02" -> Ok Black
        | "03" -> Ok ``American Indian, Aleutian, or Eskimo (includes all indigenous populations of the Western Hemisphere)``
        | "04" -> Ok Chinese
        | "05" -> Ok Japanese
        | "06" -> Ok Filipino
        | "07" -> Ok Hawaiian
        | "08" -> Ok Korean
        | "10" -> Ok Vietnamese
        | "11" -> Ok Laotian
        | "12" -> Ok Hmong
        | "13" -> Ok ``Kampuchean (Cambodian)``
        | "14" -> Ok Thai
        | "15" -> Ok ``Asian Indian or Pakistani, NOS (code 09 prior to Version 12)``
        | "16" -> Ok ``Asian Indian``
        | "17" -> Ok Pakistani
        | "20" -> Ok ``Micronesian, NOS``
        | "21" -> Ok ``Chamorro or Chamoru``
        | "22" -> Ok ``Guamanian, NOS``
        | "25" -> Ok ``Polynesian, NOS``
        | "26" -> Ok Tahitian
        | "27" -> Ok Samoan
        | "28" -> Ok Tongan
        | "30" -> Ok ``Melanesian, NOS``
        | "31" -> Ok ``Fiji Islander``
        | "32" -> Ok ``New Guinean``
        | "96" -> Ok ``Other Asian, including Asian, NOS and Oriental, NOS``
        | "97" -> Ok ``Pacific Islander, NOS``
        | "98" -> Ok Other
        | "99" -> Ok Unknown
        | "A" -> Ok ``REFUSED TO ANSWER``
        | "B" -> Ok ``PT NOT AVAILABLE``
        | "C" -> Ok SOMALI
        | "D" -> Ok ``ASIAN CAMBODIAN``
        | "E" -> Ok NEPALI
        | "F" -> Ok ``AFRICAN OTHER``
        | "G" -> Ok ``NTV HAWAIIAN or PCF ISL``
        | "H" -> Ok ``MIDDLE EASTERN NORTHERN AFRICAN``
        | "I" -> Ok ``ASIAN LAOTIAN``
        | "J" -> Ok ``MORE THAN ONE RACE``
        | _ -> Error ("Race code not found: " + code)

module Ethnicity =
    type Ethnicity =
        private
        | ``Non-Spanish; Non-Hispanic``
        | ``Mexican (includes Chicano)``
        | ``Puerto Rican``
        | Cuban
        | ``South or Central American (except Brazil)``
        | ``Other specified Spanish or Hispanic origin``
        | ``Spanish, NOS; Hispanic, NOS; Latino, NOS``
        | ``Spanish surname only``
        | ``Dominican Republic``
        | Unknown
        | ``REFUSED TO ANSWER``
        | ``PATIENT NOT AVAILABLE``
        | ``ASHKENAZI JEW``

    /// Create a patient ethnicity from a code
    let fromCode code =
        match code with
        | "00" -> Ok ``Non-Spanish; Non-Hispanic``
        | "01" -> Ok ``Mexican (includes Chicano)``
        | "02" -> Ok ``Puerto Rican``
        | "03" -> Ok Cuban
        | "04" -> Ok ``South or Central American (except Brazil)``
        | "05" -> Ok ``Other specified Spanish or Hispanic origin``
        | "06" -> Ok ``Spanish, NOS; Hispanic, NOS; Latino, NOS``
        | "07" -> Ok ``Spanish surname only``
        | "08" -> Ok ``Dominican Republic``
        | "09" -> Ok Unknown
        | "A" -> Ok ``REFUSED TO ANSWER``
        | "B" -> Ok ``PATIENT NOT AVAILABLE``
        | "C" -> Ok ``ASHKENAZI JEW``
        | _ -> Error ("Ethnicity code not found: " + code)

    let toCode ethnicity =
        match ethnicity with
           | ``Non-Spanish; Non-Hispanic`` -> "00"
           | ``Mexican (includes Chicano)`` -> "01"
           | ``Puerto Rican`` -> "02"
           | Cuban -> "03"
           | ``South or Central American (except Brazil)`` -> "04"
           | ``Other specified Spanish or Hispanic origin`` -> "05"
           | ``Spanish, NOS; Hispanic, NOS; Latino, NOS`` -> "06"
           | ``Spanish surname only`` -> "07"
           | ``Dominican Republic`` -> "08"
           | Unknown -> "09"
           | ``REFUSED TO ANSWER`` -> "A"
           | ``PATIENT NOT AVAILABLE`` -> "B"
           | ``ASHKENAZI JEW`` -> "C"

/// The patient's vital status: whether they're alive or dead.
module VitalStatus =
    type VitalStatus =
        private
        | Alive
        | Dead

    /// Create the patient's vital status from a code
    let fromCode code =
        match code with
        | "A" -> Ok Alive
        | "D" -> Ok Dead
        | _ -> Error ("Vital status code not found: " + code)

    let toCode vitalStatus =
        match vitalStatus with
        | Alive -> "A"
        | Dead -> "D"

module Patient =
    open Common
    open Gender
    open Race
    open VitalStatus

    type BirthDate = BirthDate of string

    type Patient =
        { MRN: MRN
          TccId: TccId
          AvatarId: AvatarId option
          Gender: Gender
          Race: Race
          BirthDate: BirthDate
          VitalStatus: VitalStatus }

