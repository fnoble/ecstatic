#define STM32F4

/* piksi_firmware */
#include "acq.c"
#include "cw.c"
#include "error.c"
#include "init.c"
//#include "main.c" // GIT_VERSION
#include "manage.c"
#include "nmea.c"
#include "position.c"
//#include "rtcm.c" // see below
#include "sbp.c"
#include "sbp_fileio.c"
#include "settings.c"
#include "simulator.c"
#include "simulator_data.c"
#include "solution.c"
#include "system_monitor.c"
#include "timing.c"
#include "track.c"
#include "base_obs.c"

/* libswiftnav */
#include "almanac.c"
#include "amb_kf.c"
#include "baseline.c"
#include "ambiguity_test.c"
#include "bits.c"
#include "coord_system.c"
#include "correlate.c"
#include "dgnss_management.c"
#include "edc.c"
#include "ephemeris.c"
#include "gpstime.c"
#include "lambda.c"
#include "linear_algebra.c"
#include "memory_pool.c"
#include "nav_msg.c"
#include "prns.c"
#include "pvt.c"
//#include "rtcm3.c" // struct error?
#include "sats_management.c"
#include "sbp.c"
#include "sbp_utils.c"
//#include "track.c" // collides with other
#include "tropo.c"

