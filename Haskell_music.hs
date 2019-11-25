type Group = String
type Song = String
type Album = String
type Filtered = Int
data Playlist = Playlist {
	song::[Song],
	group::[Group],
	album::[Album]}
	deriving (Show, Eq)
 
addSong :: Playlist -> Playlist -> Playlist
addSong (Playlist oldsongs oldgroups oldalbums) (Playlist newsong itsgroup itsalbum) = 
		(Playlist (oldsongs ++ newsong)  (oldgroups ++ itsgroup)  (oldalbums ++ itsalbum) )
		


serchByGroup :: Group -> Playlist -> [Filtered]
serchByGroup  group (Playlist songs groups albums) =  helper (lengtn songs) groups group    where
														helper :: Int -> [Groups] -> Eq => Group -> Playlist 
														helper (-1) groups group = Show "No such group in playlist"
															helper n (x:xs) group = if (x == group) then (Playlist (get n songs) (get n groups) (get n albums))
																else helper (n-1) groups group

