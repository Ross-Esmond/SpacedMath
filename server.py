from flask import Flask
from flask_restful import Resource, Api, reqparse, request
from generate import problem 

application = Flask(__name__)
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

