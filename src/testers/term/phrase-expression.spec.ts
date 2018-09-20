/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { Map } from 'immutable';

import { fromMatch, ParserConfig, PhraseExpression } from '../../index';

import { TesterConfig } from '../config';
import { PhraseExpressionTester } from './phrase-expression';

const pConfig = ParserConfig.create({});
const tConfig = TesterConfig.create({});
const phrase = (val: string) => PhraseExpression.fromTerm(fromMatch(pConfig)(val));

const getTester = (expr: PhraseExpression<any>) =>
  new PhraseExpressionTester(expr, tConfig);

describe('SearchQL testers', () => {

  describe('PhraseExpressionTester', () => {

    // tslint:disable-next-line:no-unnecessary-type-assertion
    const values = Map([
      'All good',
      'asdffa SDFAS sdf',
      ')((',
      'AND OR OR AND',
      'IpsUM-dolor_sitAMET',
      'hello world',
    ].map((val, i) => [`label ${i}`, val])) as Map<string, string>;

    const expressions = values.toArray()
      .map((val, i) =>
        i % 3 === 0 ?
          val.toLowerCase() :
          i % 3 === 1 ?
            val.toLocaleUpperCase() :
            val)
      .map(val => val.substr(-6, 5))
      .concat(values.toArray())
      .map(phrase);

    const notMatchingExpressions = values.toArray()
      .map((val, i) => `${i} ${val}`)
      .map(phrase);

    expressions.forEach(expression => {
      it(`should find expression "${expression}"`, () => {
        expect(getTester(expression).test(values).matches().isSome()).to.be.true;
        expect(getTester(expression).test(values).matches().some().isEmpty()).to.be.false;
      });
    });

    notMatchingExpressions.forEach(expression => {
      it(`should not find expression "${expression}"`, () => {
        expect(getTester(expression).test(values).matches().isSome()).to.be.false;
      });
    });

    it('should find expression "aaa" in few fields', () => {
      expect(
        getTester(phrase('aaa')).test(Map({
          one: 'aaa bbb aaa aaa aaasda ddaaa',
          three: 'aaGaa bbb aadaaXaadaa ddddadd',
          two: 'aaa bbb aaaXaaa',
        })).matches().toString(),
      ).to.equal('Just(Map {' +
        ' "one": Match "aaa bbb aaa aaa aaasda ddaaa" {' +
          ' Map { "aaa": OrderedSet { [0, 3], [8, 11], [12, 15], [16, 19], [25, 28] } } },' +
        ' "two": Match "aaa bbb aaaXaaa" {' +
          ' Map { "aaa": OrderedSet { [0, 3], [8, 11], [12, 15] } } }' +
        ' })');
    });

  });

});
