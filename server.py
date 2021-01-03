from flask import Flask
from flask_restful import Resource, Api, reqparse, request
from flask_cors import CORS
from generate import problem 

application = Flask(__name__)
CORS(application)
api = Api(application)

class Problems(Resource):
    def get(self):
        return { 'hello': 'world' }
    def post(self):
        return {
                'problem': problem(request.json['required'], request.json['allowed']) 
        }

api.add_resource(Problems, '/')

if __name__ == '__main__':
    application.run(debug=True)

