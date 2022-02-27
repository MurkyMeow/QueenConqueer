const express = require("express");
const path = require("path");

module.exports = function (app) {
  app.use("/static", express.static(path.join(__dirname, "static")));
};
