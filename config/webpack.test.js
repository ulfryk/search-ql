const path = require('path');

const CircularDependencyPlugin = require('circular-dependency-plugin');

module.exports = {
  mode: 'development',
  module: { rules: [{ test: /\.ts$/, loader: 'ts-loader', options: { configFile: 'tsconfig.json' } }] },
  plugins: [
    new CircularDependencyPlugin({ exclude: /node_modules/, failOnError: true }),
  ],
  resolve: {
    modules: [path.resolve(__dirname, 'src'), 'node_modules'],
    extensions: ['.ts', '.js'],
  },
};
