const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const webpack = require('webpack');

module.exports = {
    entry: path.resolve(__dirname, 'src/dev.js'),
    output: {
        path: path.resolve(__dirname, 'dist'),
        filename: 'bundle.js'
    },
    resolve: {
        modules: [
            path.resolve(__dirname, 'node_modules'),
            path.resolve(__dirname, 'bower_components')
        ]
    },
    module: {
        rules: [
            {
                test: /\.js$/,
                exclude: /(node_modules|bower_components)/,
                use: {
                    loader: 'babel-loader'
                }
            },
            {
                test: /\.html$/,
                use: [
                    {
                        loader: 'babel-loader'
                    },
                    'polymer-webpack-loader'
                ]
            },
            {
                test: /\.elm$/,
                use: [
                    {
                        loader: 'elm-hot-loader'
                    },
                    {
                        loader: 'elm-webpack-loader',
                        options: {
                            debug: true,
                            warn: true
                        }
                    }
                ]
            },
            {
                test: /\.scss$/,
                exclude: [
                    /elm-stuff/, /node_modules/
                ],
                loaders: ["style-loader", "css-loader", "sass-loader"]
            },
            {
                test: /\.css$/,
                exclude: [
                    /elm-stuff/, /node_modules/
                ],
                loaders: ["style-loader", "css-loader"]
            },
            {
                test: /\.(gif|png|jpe?g|svg)$/i,
                use: [
                    'file-loader?name=[name].[ext]&outputPath=/images/',
                    {
                        loader: 'image-webpack-loader',
                        options: {
                            bypassOnDebug: true
                        }
                    }
                ]
            }
        ]
    },
    devServer: {
        compress: true,
        hot: true,
        contentBase: path.resolve(__dirname, 'dist'),
        stats: 'errors-only',
        host: 'localhost',
        public: 'localhost:8888',
        port: 8888,
    },
    plugins: [
        new HtmlWebpackPlugin({
            template: './src/index.ejs',
            inject: 'body'
        }),
        new CopyWebpackPlugin([{
            from: path.resolve(__dirname, 'bower_components/webcomponentsjs/*.js'),
            to: 'bower_components/webcomponentsjs/[name].[ext]'
        }, {
            from: path.resolve(__dirname, 'src/static')
        }]),
        new webpack.NamedModulesPlugin(),
        new webpack.HotModuleReplacementPlugin(),
        new webpack.NoEmitOnErrorsPlugin()
    ]
};
