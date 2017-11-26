-- This file contains the core functionality of running a game of LuaBattleBots

function minsleeptime(a, b)
  return a.sleeptime < b.sleeptime and a or b
end

local nextbot = table.aggregate(bots, minsleeptime)

return nextbot
