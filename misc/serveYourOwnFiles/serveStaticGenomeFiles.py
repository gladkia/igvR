from flask import Flask
from flask_cors import CORS
app = Flask(__name__, static_url_path='/static')
CORS(app)
@app.route('/')
def serveStaticFiles():
    return 'CORS and byte-range request flask webserver for igvR and igvShiny'

if __name__ == "__main__":
    app.run(host='0.0.0.0', port='60050')
