/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { Map, OrderedSet } from 'immutable';
import * as _ from 'lodash';

import { AndOperator, BinaryOperationExpression, Operator, OrOperator, TextExpression } from '../ast';
import { SyntaxConfig } from '../config';
import { BinaryOperationExpressionTester } from './binary-operation-expression';
import { TextExpressionTester } from './text-expression';

const config = new SyntaxConfig();

const getTester = (operator: Operator, values: string[]) => {
  const expr = new BinaryOperationExpression(
    operator,
    values.map(TextExpression.fromMatch) as [TextExpression, TextExpression]);

  return new BinaryOperationExpressionTester(
    expr,
    OrderedSet(expr.value)
      .map((child: TextExpression) => new TextExpressionTester(child, config))
      .toOrderedSet(),
    config);
};

describe('SearchQL testers', () => {

  describe('BinaryOperationExpressionTester', () => {

    // tslint:disable-next-line:no-unnecessary-type-assertion
    const values = Map([
      'All good',
      'asdffa SDFAS sdf',
      ')((',
      'AND OR OR AND',
      'IpsUM-dolor_sitAMET',
      'hello world',
    ].map((val, j) => [`label ${j}`, val.toLowerCase()])) as Map<string, string>;

    const matchingTesters = [
      getTester(AndOperator.one, ['All', 'good']),
      getTester(AndOperator.one, ['asdffa', 'SDFAS']),
      getTester(AndOperator.one, ['AND', 'OR']),
      getTester(OrOperator.one, ['dolor_sitAMET', 'world']),
      getTester(OrOperator.one, ['xyz', 'ello wo']),
    ];

    const notMatchingTesters = [
      getTester(AndOperator.one, ['All', 'is good']),
      getTester(AndOperator.one, ['asdffa', 'SDFASx']),
      getTester(AndOperator.one, ['AND', 'OOxx']),
      getTester(OrOperator.one, ['dolor--sitAMET', 'worldx']),
      getTester(OrOperator.one, ['xyz', 'ello woxx']),
    ];

    matchingTesters.forEach(tester => {
      it(`should find expression "${tester.ast}"`, () => {
        expect(tester.test(values).isSome()).to.be.true;
        expect(tester.test(values).some().isEmpty()).to.be.false;
      });
    });

    notMatchingTesters.forEach(tester => {
      it(`should not find expression "${tester.ast}"`, () => {
        expect(tester.test(values).isSome()).to.be.false;
      });
    });

    it('should build proper Match output', () => {
      expect(String(matchingTesters[1].test(values)))
        .to.equal('Just(Map { "label 1": Match "asdffa sdfas sdf" { Map {' +
        ' "asdffa": OrderedSet { [0, 6] },' +
        ' "sdfas": OrderedSet { [7, 12] }' +
        ' } } })');

      expect(String(matchingTesters[3].test(values)))
        .to.equal('Just(Map { "label 4": Match "ipsum-dolor_sitamet" { Map {' +
        ' "dolor_sitamet": OrderedSet { [6, 19] }' +
        ' } } })');

      expect(String(matchingTesters[4].test(values)))
        .to.equal('Just(Map { "label 5": Match "hello world" { Map {' +
        ' "ello wo": OrderedSet { [1, 8] }' +
        ' } } })');
    });

  });

});
