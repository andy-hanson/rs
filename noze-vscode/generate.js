#! /usr/bin/env node
// @ts-check

/// <reference types="node" />

const { writeFileSync } = require('fs')
/** @type {any} */
const plist = require('plist')

const keywords = [
	"_",
	"abstract",
	"array",
	"assert",
	"builtin",
	"catch",
	"def",
	"do",
	"else",
	"enum",
	"finally",
	"for",
	"fun",
	"generic",
	"get",
	"if",
	"io",
	"is",
	"impl",
	"import",
	"in",
	"new",
	"pass",
	"recur",
	"self",
	"set",
	"slots",
	"then",
	"try",
	"val",
	"var",
	"when",
	// Operators are "keywords" too
	"\\<",
	"\\>",
	"\\<\\=",
	"\\>\\=",
	"\\+",
	"\\-",
	"\\*",
	"\\/"
]

const patterns = [
	match("keyword", /^\s*(abstract|fun|def|var|val)\s+([A-Z][a-zA-Z0-9\-]*)\s+([a-zA-Z0-9\-]*)/, {
		2: "support.type",
		3: "string.quoted",
	}),
	match("keyword", /^\s*impl\s+([A-Z][a-zA-Z0-9\-]*)\.([a-zA-Z0-9\-]*)/, {
		1: "support.type",
		2: "string.quoted",
	}),

	match("support.type", /\b[A-Z][a-zA-Z0-9\-]*\b/),
	match("keyword", new RegExp(`\\b(${keywords.join("|")})[^a-zA-Z0-9\-]`)),
	match("", /\b[a-z][a-zA-Z0-9\-]*\b/),
	match("comment", /\.|\,|=|\\/),


	section("string.quoted", /"/, /"/, {
		patterns: [
			match("keyword", /\\./), // "constant.character.escape"
			match("keyword", /\{([^}])\}/, "string.interpolated"),
		],
	}),

	match("comment", /\|\|?\s.*/),
	match("comment", /^\s*(region)[^\n]*\n/, "keyword"),
	match("comment", /^\s*(todo)[^\n]*\n/, "keyword"),

	match("comment", /[\(\[\]\)\{\}]/),
	match("constant.numeric", /[\+\-]?((0((b[01]+)|(o[0-8]+)|(x[\da-f]+)))|(\d+(\.\d+)?))/),
]

const content = {
	name: "Noze",
	scopeName: "source.noze",
	fileTypes: ["nz"],
	patterns,
}

//writeFileSync("grammars/noze.json", JSON.stringify(content, null, "\t"))
writeFileSync('syntaxes/noze.tmLanguage', plist.build(content))

/** @typedef {string | { [key: string]: string }} Captures */

/** @typedef {{}} Match */

/**
 * @param {string} name
 * @param {RegExp} match
 * @param {Captures} [captures]
 * @return {Match}
 */
function match(name, match, captures) {
	const o = {name, match: typeof match === "string" ? match : match.source}
	if (captures) o.captures = convertCaptures(captures)
	return o
}

/**
 *
 * @param {string} name
 * @param {RegExp} begin
 * @param {RegExp} end
 * @param {{ patterns?: Match[], beginCaptures?: Captures, endCaptures?: Captures }} opts
 */
function section(name, begin, end, opts) {
	const obj = {name, begin: begin.source, end: end.source}
	const {patterns, beginCaptures, endCaptures} = opts
	if (patterns)
		obj.patterns = patterns
	if (beginCaptures)
		obj.beginCaptures = convertCaptures(beginCaptures)
	if (endCaptures)
		obj.endCaptures = convertCaptures(endCaptures)
	return obj
}

/**
 * @template T, U
 * @param {{ [key: string]: T }} obj
 * @param {function(T): U} mapper
 * @return {{ [key: string]: U }}
 */
function mapObjectValues(obj, mapper) {
	const res = {}
	for (const key in obj)
		res[key] = mapper(obj[key])
	return res
}

/**
 * @param {Captures} captures
 * @return {{ [key: string]: {name: string} }}
 */
function convertCaptures(captures) {
	return typeof captures === 'string' ? { 1: {name: captures} } : mapObjectValues(captures, name => ({name}))
}
