from flask import Flask

app = Flask(__name__)

@app.route("/taskCreated")
def informTaskCreated ():
  print("informTaskCreated API Called")
  return "todo Task Created"