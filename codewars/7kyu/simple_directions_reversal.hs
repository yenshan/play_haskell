


solve :: [String] -> [String]
solve xss = map (uncurry (++)) $ zip routes roads
    where
        routes = "Begin on " : (reverse $ map parseRoute $ map words xss)
        roads = reverse $ map parseRoad $ map words xss

parseRoad ["Begin", "on", x, y] = x ++ " " ++ y
parseRoad ["Right", "on", x, y] = x ++ " " ++ y
parseRoad ["Left",  "on", x, y] = x ++ " " ++ y


parseRoute ["Begin", "on", _, _] = ""
parseRoute ["Right", "on", _, _] = "Left on"
parseRoute ["Left",  "on", _, _] = "Right on"


routeA = ["Begin on Road A","Right on Road B","Right on Road C","Left on Road D"]

routeB = ["Begin on Lua Pkwy", "Right on Sixth Alley", "Right on 1st Cr"]
