LON_RANGE <- 0.5
LAT_RANGE <- 0.5
NB2 <- c(0, 1)
INFO_STRING <- "Created with the CM SAF R Toolbox."

UNDEFINED <- "undefined"
STANDARD <- "standard"

UNITS <- list(
  ONE = "1",
  UNDEFINED = "undefined",
  DEGREES_NORTH = "degrees_north",
  DEGREES_EAST = "degrees_east",
  KILOMETER = "km"
)

AXIS <- list(
  X = "X",
  Y = "Y",
  Z = "Z",
  TIME = "T"
)

NB2_NAME <- "nb2"

LON_NAMES <- list(
  LONG_DEFAULT = "longitude",
  "Longitude",
  "Lon",
  DEFAULT = "lon"
)

LAT_NAMES <- list(
  LONG_DEFAULT = "latitude",
  "Latitude",
  "Lat",
  DEFAULT = "lat"
)

Y_NAMES <- list(
  DEFAULT = "y",
  "Y",
  LONG_DEFAULT = "y-coordinate in kilometer",
  "Y-coordinate in kilometer"
)

X_NAMES <- list(
  DEFAULT = "x",
  "X",
  LONG_DEFAULT = "x-coordinate in kilometer",
  "X-coordinate in kilometer"
)

TIME_NAMES <- list(
  DEFAULT = "time",
  "Time"
)

TIME_BOUNDS_NAMES <- list(
  "tb",
  "Tb",
  DEFAULT = "time_bnds",
  "Time_bnds",
  "bndsize"
)

DIM_NAMES <- c(
  NB2_NAME,
  TIME_BOUNDS_NAMES,
  LON_NAMES,
  LAT_NAMES,
  X_NAMES,
  Y_NAMES,
  TIME_NAMES
)

PRECISIONS_VAR <- list(
  SHORT = "short",
  INTEGER = "integer",
  FLOAT = "float",
  DOUBLE = "double",
  CHAR = "char",
  BYTE = "byte"
)

PRECISIONS_ATT <- list(
  SHORT = "short",
  FLOAT = "float",
  DOUBLE = "double",
  TEXT = "text"
)

GLOBAL_ATT_DEFAULT <- c(
  "creator_email",
  "creator_name",
  "creator_type",
  "creator_url",
  "dataset_version",
  "date_created",
  "geospatial_lat_resolution",
  "geospatial_lat_units",
  "geospatial_lon_resolution",
  "geospatial_lon_units",
  "id",
  "institution",
  "instrument",
  "instrument_vocabulary",
  "keywords",
  "keywords_vocabulary",
  "platform",
  "platform_vocabulary",
  "producer",
  "product_version",
  "project",
  "publisher_email",
  "publisher_name",
  "publisher_type",
  "publisher_url",
  "references",
  "source",
  "standard_name_vocabulary",
  "summary",
  "title",
  "version"
)

ATTR_NAMES <- list(
  STANDARD_NAME = "standard_name",
  LONG_NAME = "long_name",
  CMSAF_INFO = "cmsaf_info",
  FILL_VALUE = "_FillValue",
  MISSING_VALUE = "missing_value",
  CALENDAR = "calendar",
  BOUNDS = "bounds",
  AXIS = "axis",
  INFO = "Info",
  UNITS = "units"
)
