#' Complete list of palettes
#'
#' Use \code{\link{nba_palette}} to construct palettes of desired length.
#'
#' @export
nba_palettes <- list(
  bobcats = c('#0C2340', '#418FDE', '#1B365D', '#E35205', '#888B8D'),
  bobcats_original = c('#F9423A', '#1B365D', '#8D9093', '#010101'),
  blazers = c('#E13A3E', '#C4CED4', '#000000'),
  blazers_statement = c('#C8102E', '#010101', '#373A36'),
  blazers_city = c('#fea30c', '#203b7e', '#0881c6', '#cf152d', '#df4826'),
  blazers_city2 = c('#2E2E2E', '#140C0B', '#C72830', '#EC3036'),
  bucks = c('#00471B', '#EEE1C6', '#0077C0', '#000000'),
  bucks_earned = c('#7AC043', '#00713D', '#014711', '#CB0423', '#D7D7D7', '#FFFFFF'),
  bucks_00s = c('#AC1A2F', '#274E37', '#95999D'),
  bucks_retro = c('#2C5234', '#00843D', '#6CC24A', '#DE7C00', '#010101'),
  bucks_city = c('#DDCBA4', '#2C5234', '#0057B7', '#010101'),
  bucks_city2 = c('#FFC72C', '#FFA400', '#DA291C', '#6CC24A', '#2C5234',
                  '#0057B7'),
  bulls = c('#CE1141', '#000000'),
  bulls_city = c('#333F48', '#C6AA76', '#BA0C2F'),
  bulls_city2 = c('#010101', '#69B3E7', '#BA0C2F'),
  bulls_holiday = c('#007A33', '#010101', '#BA0C2F'),
  cavaliers = c('#860038', '#041E42', '#FDBB30', '#000000'),
  cavaliers_90s = c('#E35205', '#5C88DA', '#27251F'),
  cavaliers_retro = c('#DC3B34', '#04225C'),
  celtics = c('#008348', '#061922', '#bb9753'),
  celtics2 = c('#007A33', '#BA9653', '#963821', '#E59E6D', '#000000'),
  celtics_europe = c('#007A33', '#010101', '#BA0C2F'),
  celtics_champ = c('#B4975A', '#7A673B', '#010101', '#007A33'),
  clippers = c('#C8102E', '#1D428A', '#BEC0C2', '#000000'),
  clippers_city = c('#003DA5', '#418FDE', '#DC4405', '#B1B3B3', '#010101'),
  clippers_retro = c('#418FDE','#003DA5', '#D50032'),
  clippers_original = c('#00B398', '#010101', '#FF8200', '#8D9093'),
  grizzlies = c('#5D76A9', '#12173F', '#F5B112', '#707271', '#F21A00'),
  grizzlies_00s = c('#707271', '#00285E', '#FDB927', '#BED4E9'),
  grizzlies_retro = c('#00B2A9', '#E43C40', '#BC7844', '#040204'),
  grizzlies_europe = c('#0C2340', '#7D9CC0', '#CED9E5', '#A6192E',
                       '#FFC72C'),
  hawks = c('#C8102E', '#FDB927', '#9EA2A2', '#000000'),
  hawks_statement = c('#C8102E', '#C1D32F', '#26282A'),
  hawks_90s = c('#C8102E', '#FFCD00', '#87674F', '#000000'),
  hawks_retro = c('#041E42', '#69B3E7', '#C8102E'),
  heat = c('#98002E', '#F9A01B', '#000000'),
  heat_90s = c('#BA0C2F', '#FEDD00', '#000000'),
  heat_vice = c('#41B6E6', '#DB3EB1', '#000000'),
  heat_dark = c('#010101', '#BA0C2F', '#E03C31', '#FFCD00'),
  heat_0506 = c('#D40F7D', '#E35205', '#000000'),
  heat_military = c('#B7B09C', '#535435', '#4E4934', '#A6192E', '#0C2340'),
  hornets = c('#00778B', '#211747', '#888B8D'),
  hornets2 = c('#00778B', '#211747', '#888B8D', '#6CACE4'),
  hornets_classic = c('#280071', '#0032A0','#00778B', '#6CACE4',
                      '#F9423A', '#010101'),
  hornets_city = c('#BDE9C9', '#25282A', '#C6AA76'),
  hornets_city2 = c('#B1B3B3', '#888B8D', '#211747', '#00778B'),
  huskies = c('#003DA5', '#FFFFFF'),
  jazz = c('#0C2340', '#2C5234', '#FFA400'),
  jazz_city = c('#010101', '#5D2A2C', '#93282C', '#C8102E',
                '#DA291C', '#DC582A', '#E87722', '#FF9E1B',
                '#FFC72C'),
  jazz_classic = c('#582C83', '#00A9E0', '#006271', '#954E4C', '#010101'),
  jazz_retro = c('#753BBD', '#00A9E0', '#006271', '#954E4C', '#010101'),
  kings = c('#582C83', '#5B6770', '#010101'),
  kings_city = c('#010101', '#545859', '#418FDE', '#D50032'),
  kings_alt = c('#753BBD', '#8D9093', '#010101', '#003DA5', '#D50032'),
  kings_alt2 = c('#010101', '#582C83', '#5B6770'),
  knicks = c('#003DA5', '#FF6720', '#B1B3B3'),
  knicks_retro = c('#003DA5', '#E35205', '#010101'),
  knicks_city = c('#010101', '#003DA5', '#FF6720', '#B1B3B3'),
  knicks_city2 = c('#0C2340', '#FF6720', '#707372', '#C8C9C7'),
  knicks_holiday = c('#007A33', '#FE5000', '#0072CE', '#8D9093', '#010101'),
  lakers = c('#582C83', '#FFC72C', '#010101'),
  lakers_city = c('#010101', '#373A36', '#FFC72C', '#582C83'),
  lakers_alt = c('#702F8A', '#9063CD', '#FFC72C'),
  magic = c('#0057B7', '#8D9093', '#010101'),
  magic_city = c('#FF8200', '#E57200', '#010101'),
  magic_city2 = c('#3B4559', '#1B365D', '#0057B7', '#CCCCCC', '#010101'),
  mavericks = c('#0050B5', '#0C2340', '#8D9093', '#010101'),
  mavericks_alt = c('#007A33', '#0057B7', '#0C2340', '#8D9093', '#010101'),
  mavericks_city = c('#0086BF', '#0C2340', '#010101', '#78BE21'),
  mavericks_retro = c('#002855', '#FA4616', '#010101'),
  mavericks_banner = c('#041E42', '#0057B7', '#8D9093', '#FED141',
                       '#AD841F'),
  nets = c('#707372', '#010101'),
  nets_city = c('#010101', '#FFB81C', '#DA291C', '#004C97', '#418FDE'),
  hornets_believe = c('#211747', '#004F71', '#59C7EB', '#99D6EA', '#0082BA',
                      '#FFC72C'),
  nuggets = c('#0C2340', '#1D4289', '#862633', '#FFC72C'),
  nuggets_80s = c('#041E42', '#041E42', '#FFCD00', '#E4002B',
                  '#FF6720', '#0032A0', '#AC145A'),
  nuggets_statement = c('#AC145A', '#FFC72C', '#862633', '#0C2340'),
  nuggets_city = c('#862633', '#862633', '#FC4C02', '#FF8200', '#FFC72C'),
  nuggets_city2 = c('#010101', '#C8102E', '#FF6720', '#FFC72C', '#009739',
                    '#1D4289', '#702F8A'),
  pacers = c('#041E42', '#FFC72C', '#B1B3B3'),
  pacers_classic = c('#FFA400', '#862633'),
  pacers_venue = c('#782F40', '#004E42', '#F2C75C', '#010101'),
  pacers_foundation = c('#0047BB', '#EFD19F', '#5B7F95', '#FFB81C'),
  pelicans = c('#0C2340', '#B9975B', '#C8102E'),
  pelicans_city = c('#1D4289', '#C8102E', '#B9975B', '#89734C'),
  pelicans_pride = c('#211747', '#2C5234', '#FFB81C', '#89734C'),
  pistons = c('#041E42', '#1D4289', '#C8102E', '#B1B3B3'),
  pistons_90s = c('#006271', '#9D2235', '#FFA400', '#8D9093', '#010101'),
  pistons_city = c('#1D4289', '#041E42', '#C8102E', '#B1B3B3'),
  raptors = c('#BA0C2F', '#753BBD', '#B9975B', '#89734C', '#010101'),
  raptors_statement = c('#BA0C2F', '#545859', '#010101'),
  raptors_city = c('#888B8D', '#B9975B', '#89734C', '#010101'),
  raptors_original = c('#753BBD', '#BA0C2F', '#8D9093', '#010101'),
  raptors_europe = c('#BA0C2F', '#00A05E', '#8D9093', '#010101'),
  raptors_military = c('#010101', '#8D9093', '#789D4A', '#3D441E', '#816040'),
  rockets = c('#BA0C2F', '#9EA2A2', '#373A36', '#010101'),
  rockets_90s = c('#041E42', '#BA0C2F', '#2C7AA1', '#8D9093'),
  rockets_city = c('#0C2340', '#418FDE', '#BA0C2F'),
  rockets_original = c('#FFC72C', '#BA0C2F', '#010101'),
  sixers = c('#0C2340', '#003DA5', '#D50032', '#8D9093', '#010101'),
  sixers_retro = c('#012169', '#C8102E'),
  sixers_90s = c('#010101', '#8D9093', '#D50032', '#896C4C'),
  sixers_city = c('#888B8D', '#E4D5D3', '#003DA5', '#D50032'),
  spurs = c('#9A8822', '#F5CDB4', '#F8AFA8', '#FDDDA0', '#74A089'),
  suns = c('#211747', '#FFA400', '#CB6015', '#A9431E', '#B1B3B3',
           '#5B6770', '#010101'),
  suns_00s = c('#582C83', '#CB6015', '#FFA400', '#A6192E', '#5B6770', '#010101'),
  suns_retro = c('#702F8A', '#FA4616', '#A45A2A'),
  suns_statement = c('#010101', '#5B6770', '#B1B3B3', '#CB6015', '#211747'),
  suns_city = c('#010101', '#211747', '#582C83', '#5F249F', '#702F8A', '#A6192E',
                '#C8102E', '#FC4C02', '#FF6720', '#FF7F41', '#FFA400',
                '#FFB81C', '#FFCD00'),
  supersonics = c('#173F35', '#FFA400', '#010101'),
  supersonics_90s = c('#173F35', '#9E2B2F', '#FFA400', '#8F654D', '#E35205'),
  supersonics_holiday = c('#D50032', '#F6BE00', '#00573F', '#010101'),
  thunder = c('#0072CE', '#041E42', '#F9423A', '#FFB81C'),
  thunder_statement = c('#041E42', '#0072CE', '#0072CE'),
  thunder_city = c('#25282A', '#545859', '#9EA2A2', '#FFB81C',
                   '#F9423A', '#0072CE'),
  thunder_city2 = c('#0093B2', '#0072CE', '#041E42', '#F9423A', '#FFB81C'),
  thunder_tribute = c('#373A36', '#C27237',
                      '#007A33', '#007A33', '#FFCD00', '#0072CE', '#74531C',
                      '#F9423A', '#041E42'),
  timberwolves = c('#0C2340', '#236192', '#78BE21', '#9EA2A2'),
  timberwolves_00s = c('#236192', '#009739', '#FFD700',
                       '#C8102E', '#8D9093', '#010101'),
  timberwolves_classic = c('#010101', '#8D9093', '#236192', '#00843D',
                           '#FFD700', '#C8102E'),
  timberwolves_statement = c('#C8102E', '#236192', '#0C2340', '#9EA2A2'),
  warriors = c('#003DA5', '#FFC72C', '#25282A'),
  warriors_00s = c('#041E42', '#00A9E0', '#BE3A34', '#FFA400'),
  warriors_city = c('#041E42', '#BE3A34', '#FFA400'),
  warriors_city2 = c('#003DA5', '#2CCCD3', '#FFC72C', '#D50032'),
  warriors_cny = c('#25282A', '#D50032', '#FFC72C'),
  wizards = c('#010101', '#0C2340', '#8D9093'),
  wizards_city = c('#B1B3B3', '#0C2340', '#C8102E'),
  wizards_earned = c('#C8102E', '#0C2340', '#8D9093', '#C8C9C7')
)


#' Palette options
#' Generate a tibble of with palette options
#'
#' @param team return all available colors for a given team. Default is
#' all available teams
#' @return A tibble containing the palette options for a given NBA team.
#'
#' @export
#' @keywords colors
#' @examples
#' available_palettes()#'
#' available_palettes('raptors')#'
#' available_palettes('heat')

available_palettes <- function(team = 'all') {

  team_names <- sapply(strsplit(names(nba_palettes), "_"), '[[', 1)
  palette_names = names(nba_palettes)
  palette_colors = sapply(nba_palettes, function(x) {
    paste(x, collapse = ", ")
  })

  if(team == 'all') {
    palette_options <- dplyr::tibble(teams = team_names,
                                     palette_names = palette_names,
                                     palette_colors = palette_colors)

    return(palette_options)
  } else {

    palette_options <- dplyr::tibble(teams = team_names[which(team_names == team)],
                                     palette_names = palette_names[which(team_names == team)],
                                     palette_colors = palette_colors[which(team_names == team)])
    return(palette_options)
  }

}


#' An NBA  palette generator
#'
#' Color palettes associated with a large variety of NBA team jerseys.
#'
#' @param n Number of colors desired. Most palettes contain 3-5 colors although
#' some contain as many as 8-10.  All color schemes are derived from the
#' following amazing resource:
#'   \href{https://www.trucolor.net/portfolio/national-basketball-association-franchise-records-1946-1947-through-present/}{TruColor}.
#'   If omitted, uses all colours.
#' @param name Name of desired palette. use `available_palettes()` to see which
#' options are available
#' @param type Either "continuous" or "discrete". Use continuous if you want
#'   to automatically interpolate between colours.
#'   @importFrom graphics rgb rect par image text
#' @return A vector of colours.
#' @export
#' @keywords colors
#' @examples
#' nba_palette("raptors")
#' nba_palette("jazz_city")
#' nba_palette("nuggets_80s")
#'
#' # If you need more colours than normally found in a palette, you
#' # can use a continuous palette to interpolate between existing
#' # colours
#' pal <- wes_palette(21, name = "Zissou1", type = "continuous")
#' image(volcano, col = pal)
nba_palette <- function(name,  n, type = c("discrete", "continuous")) {
  type <- match.arg(type)

  name_check <- setdiff(name, names(nba_palettes))

  if(length(name_check) > 0) {
    stop(paste(name_check, 'is not a valid pallete name'))
  }

  nm <- paste(name, collapse = '-')
  pal <- unique(unlist(nba_palettes[name]))

  if (is.null(pal))
    stop("Palette not found.")

  if (missing(n)) {
    n <- length(pal)
  }

  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }

  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = nm)
}

#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}
