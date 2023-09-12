from flask import Flask

app = Flask(__name__)

@app.route("/taskCreated")
def informTaskCreated ():
  return "todo Task Created"