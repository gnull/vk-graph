import Vk (getFriends, getUser)

main = getUser 210700286 >>= getFriends >>= print
