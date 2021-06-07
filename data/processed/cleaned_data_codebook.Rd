\name{cleaned_data_set}
\alias{cleaned_data_set}
\docType{data}

\title{
Collection of Award Nominations and Wins for Films Released 2000–2017
}
\description{
describe here
}
\usage{cleaned_data_set <- readRDS("processed/cleaned_data_set.rds")}
\format{
  A data frame with 1183 observations across 119 variables.
  \describe{
    \item{\code{id}}{an integer variable identifying each song. This is directly corrected to the row number of a given song.}
    \item{\code{danceability}}{a numeric variable ranging from 0.0 to 1.0 describing how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity.}
    \item{\code{energy}}{a numeric variable ranging from 0.0 to 1.0 representing a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low on the scale.}
    \item{\code{key}}{a numeric variable estimating the overall key of the track. Integers map to pitches using standard Pitch Class notation . E.g. 0 = C, 1 = C♯/D♭, 2 = D, and so on.}
    \item{\code{loudness}}{a numeric variable representing the overall loudness of a track in decibels (dB). Loudness values are averaged across the entire track and are useful for comparing relative loudness of tracks.}
    \item{\code{mode}}{a categorical variable indicating the modality (major or minor) of a track, the type of scale from which its melodic content is derived.}
    \item{\code{speechiness}}{a numeric variable ranging from 0.0 to 1.0 detecting the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value.}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Molina, A. (2019, April). Spotify music genre list and 80k songs/tracks, Version 6. Retrieved January 12, 2021 from https://www.kaggle.com/grasslover/spotify-music-genre-list/version/6.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(blah)
## maybe str(blah) ; plot(blah) ...
}
\keyword{301final}