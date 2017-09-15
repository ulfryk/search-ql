const path = require('path');

const CircularDependencyPlugin = require('circular-dependency-plugin');

module.exports = {
  module: { rules: [{ test: /\.ts$/, loader: 'ts-loader', options: { configFile: 'tsconfig.json' } }] },
  resolve: {

    modules: [path.resolve(__dirname, 'src'), 'node_modules'],

    extensions: ['.ts', '.js'],

    plugins: [
      new CircularDependencyPlugin({ exclude: /node_modules/, failOnError: true }),
    ],

  },
};
