\name{cleaned_data_set {301final}}
\alias{cleaned_data_set}
\docType{data}

\title{
Collection of Award Nominations and Wins for Films
}
\description{
The data was sourced from the BigML platform which contained IMDb data scrapes, and it comprises 119 different statistics about films and
their respective film nominations and awards for 1183 pieces of media (released 2000–2017).
}
\usage{cleaned_data_set <- readRDS("processed/cleaned_data_set.rds")}
\format{
  A data frame with 1183 observations on 119 variables, with a focus on the # variables required
  for the recipe building process. All variables are numeric or character except for one, which    is a factor.
  \describe{
    \item{\code{Oscar_nominated}}{the outcome variable in factor form indicating whether the film received any Oscar nominations or not, with 1+ nominations corresponding with '1' and 0 nominations corresponding with '0'.}
    \item{\code{certificate}}{a numeric variable ranging from 0.0 to 1.0 describing how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity.}
    \item{\code{duration}}{a numeric variable indicating the number of minutes of the film.}
    \item{\code{genre}}{a character variable indicating the genre of the film.}
    \item{\code{rate}}{a numeric variable indicating the IMDb rating of the film.}
    \item{\code{metascore}}{a numeric variable indicating the conglomerate score of the different famous film critic organizations.}
    \item{\code{gross}}{a numeric variable indicating the film's gross earnings.}
    \item{\code{awards_wins}}{a numeric variable indicating the number of awards the film won.}
    \item{\code{awards_nominations}}{a numeric variable indicating the number of award nominations the film received.}
    \item{\code{Golden_Globes_nominated, \cr Critics_Choice_nominated, \cr
    BAFTA_nominated}}{a character variable indicating whether or not the film was nominated for the named award.}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~ idk if necessary
}
\source{
BigML. "Movies 2000-2017." https://bigml.com/user/academy_awards/gallery/dataset/5a94302592fb565ed400103b
}
\references{
%%  ~~ possibly secondary sources and usages ~~ idk if necessary
}
\examples{
just paste the code for the ggplot or the recipe idk
}
\keyword{301final}