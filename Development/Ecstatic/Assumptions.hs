module Development.Ecstatic.Assumptions where

type Assumption = (String, Int)
num_dds :: Int
num_dds = 11
-- TODO check for missing assumptions
-- TODO check lwork
assumptions :: [Assumption]
assumptions = [("num_dds", num_dds),
               ("res_dim", 2*num_dds - 3),
               ("dd_dim", 2*num_dds),
               ("num_sats", num_dds+1),
               ("state_dim", num_dds),
               ("new_state_dim", num_dds),
               ("lwork", 22)]

-- TODO parse this from file
stack_limits :: [(String, Int)]
stack_limits =
  [ ("nap_exti", 2000)
  , ("nav_msg_thread", 3000)
  , ("manage_acq_thread", 3000)
  , ("manage_track_thread", 3000)
  , ("sbp_thread", 4096)
  , ("solution_thread", 10000)
  , ("time_matched_obs_thread", 20000)
  , ("track_status_thread", 128)
  , ("system_monitor_thread", 3000) ]

