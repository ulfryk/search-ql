/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { Map } from 'immutable';

import { TextExpression } from '../ast';
import { SyntaxConfig } from '../config';
import { TextExpressionTester } from './text-expression';

const getTester = (expr: TextExpression) =>
  new TextExpressionTester(expr, new SyntaxConfig());

describe('SearchQL testers', () => {

  describe('TextExpressionTester', () => {

    // tslint:disable-next-line:no-unnecessary-type-assertion
    const values = Map([
      'All good',
      'asdffa SDFAS sdf',
      ')((',
      'AND OR OR AND',
      'IpsUM-dolor_sitAMET',
      'hello world',
    ].map((val, i) => [`label ${i}`, val.toLowerCase()])) as Map<string, string>;

    const expressions = values.toArray()
      .map((val: string) => val.substr(-6, 5))
      .concat(values.toArray())
      .map(TextExpression.fromMatch);

    const notMatchingExpressions = values.toArray()
      .map((val, i) => `${i} ${val}`)
      .map(TextExpression.fromMatch);

    expressions.forEach(expression => {
      it(`should find expression "${expression}"`, () => {
        expect(getTester(expression).test(values).isSome()).to.be.true;
        expect(getTester(expression).test(values).some().isEmpty()).to.be.false;
      });
    });

    notMatchingExpressions.forEach(expression => {
      it(`should not find expression "${expression}"`, () => {
        expect(getTester(expression).test(values).isSome()).to.be.false;
      });
    });

    it('should find expression "aaa" in few fields', () => {
      expect(
        getTester(TextExpression.fromMatch('aaa')).test(Map({
          one: 'aaa bbb aaa aaa aaasda ddaaa',
          three: 'aaGaa bbb aadaaXaadaa ddddadd',
          two: 'aaa bbb aaaXaaa',
        })).toString(),
      ).to.equal('Just(Map {' +
        ' "one": Match "aaa bbb aaa aaa aaasda ddaaa" {' +
          ' Map { "aaa": OrderedSet { [0, 3], [8, 11], [12, 15], [16, 19], [25, 28] } } },' +
        ' "two": Match "aaa bbb aaaXaaa" {' +
          ' Map { "aaa": OrderedSet { [0, 3], [8, 11], [12, 15] } } }' +
        ' })');
    });

  });

});
