from flask import Flask
from flask_restful import Resource, Api, reqparse, request
from flask_cors import CORS
from generate import problem 

app = Flask(__name__)
CORS(app)
api = Api(app)

class Problems(Resource):
    def get(self):
        return { 'hello': 'world' }
    def post(self):
        return {
                'problem': problem(request.json['required'], request.json['allowed']) 
        }

@app.route('/options')
def options():
    return { 'options': ['mult', 'div', 'addition', 'power'] }

api.add_resource(Problems, '/')

if __name__ == '__main__':
    app.run(debug=True)

