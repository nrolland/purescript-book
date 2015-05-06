module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    srcFiles: ["src/**/*.purs", "bower_components/**/src/**/*.purs"],

    psc: {
      options: {
      main: "Chapter2",
      modules: ["Chapter2"]
      },
      all: {
      src: ["<%=srcFiles%>"],
        dest: "dist/Main.js"
      }
    },

    pscMake: {
      options: {
      main: "Chapter2",
      modules: ["Chapter2"]
      },
      all: {
      src: ["<%=srcFiles%>"],
        dest: "dist/"
      }
    },

    dotPsci: ["<%=srcFiles%>"]
  });

  grunt.loadNpmTasks("grunt-purescript");
  grunt.registerTask("default", ["pscMake:all", "dotPsci"]);
};
