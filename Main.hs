import Vk (getFriends, getUser, getFF)

main = length <$> (getUser 210700286 >>= getFF 2) >>= print
