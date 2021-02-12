from imgurpython import ImgurClient

client_id = '063028b85575f16'
client_secret = 'e73cfe7f4855faae8caeceac9374f26f0685152a'

client = ImgurClient(client_id, client_secret)


client.get_album_images("M9RfXgh")
