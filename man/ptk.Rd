% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{ptk}
\alias{ptk}
\title{Duration and voicing measures of voiceless plosives in Spanish}
\format{
A data frame with 751 rows and 11 variables:
\describe{
  \item{cdur}{Total plosive duration, measured from preceding vowel intensity
    maximum to following vowel intensity maximum, in milliseconds.}

  \item{vdur}{Duration of the period of voicelessness in the
    vowel-consonant-vowel sequence in milliseconds.}

  \item{place}{Place of articulation (Bilabial, Dental, or Velar).}

  \item{stress}{Syllabic stress context (Tonic, Post-Tonic, or Unstressed).}

  \item{prevowel}{Preceding vowel phoneme identity (a, e, i, o, or u).}

  \item{posvowel}{Following vowel phoneme identity (a, e, i, o, or u).}

  \item{wordpos}{Position of the plosive in the word (Initial or Medial).}

  \item{wordfreq}{Number of times the word containing the plosive occurs in
    the CREA corpus.}

  \item{speechrate}{Local speech rate around the consonant in nuclei per
    second.}

  \item{sex}{The speaker's sex (Female or Male).}

  \item{speaker}{Speaker identifier (s01 through s18).}
}
}
\usage{
ptk
}
\description{
A dataset containing measures of total duration and voiceless period duration
for instances of intervocalic Spanish /p/, /t/, and /k/.  The data are taken
from 18 speakers in the task dialogues in the Spanish portion of the Glissando
Corpus (the speakers are university students in Valladolid, Spain).
If you analyze the \code{ptk} dataset in a publication, please cite Eager
(2017) from the references section below.
}
\section{References}{

Eager, Christopher D. (2017). Contrast preservation and constraints on
individual phonetic variation. Doctoral thesis. University of Illinois at
Urbana-Champaign.

Garrido, J. M., Escudero, D., Aguilar, L., Cardeñoso, V., Rodero, E.,
de-la-Mota, C., … Bonafonte, A. (2013). Glissando: a corpus for
multidisciplinary prosodic studies in Spanish and Catalan. Language Resources
and Evaluation, 47(4), 945–971.

Real Academia Española. Corpus de referencia del español actual (CREA). Banco
de Datos. Retrieved from http://www.rae.es

De Jong, N. H., & Wempe, T. (2009). Praat script to detect syllable nuclei
and measure speech rate automatically. Behavior Research Methods, 41(2),
385–390.
}

\keyword{datasets}
