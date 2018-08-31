/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { Map, OrderedSet } from 'immutable';
import * as _ from 'lodash';

import { AndOperator, BinaryOperationExpression, Expression, LikeOperator, Operator, OrOperator, TermExpression } from '../ast';
import { SyntaxConfig } from '../config';
import { BinaryOperationExpressionTester, Tester } from './index';

const config = new SyntaxConfig();
const And = new AndOperator(config.AND[0]);
const Or = new OrOperator(config.OR[0]);
const Like = new LikeOperator(config.LIKE[0]);

const getTester = (operator: Operator, values: [string, string]) => {
  const [lhs, rhs] = values.map(TermExpression.fromMatch);
  const expr = BinaryOperationExpression.fromPair(operator)(lhs as any, rhs as any);

  return new BinaryOperationExpressionTester(
    expr,
    OrderedSet<Expression>(expr.value).map(Tester.fromAst(config)).toOrderedSet(),
    config);
};

describe('SearchQL testers', () => {

  describe('BinaryOperationExpressionTester', () => {

    describe('logical operations (AND, OR)', () => {

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
        getTester(And, ['All', 'good']),
        getTester(And, ['asdffa', 'SDFAS']),
        getTester(And, ['AND', 'OR']),
        getTester(Or, ['dolor_sitAMET', 'world']),
        getTester(Or, ['xyz', 'ello wo']),
      ];

      const notMatchingTesters = [
        getTester(And, ['All', 'is good']),
        getTester(And, ['asdffa', 'SDFASx']),
        getTester(And, ['AND', 'OOxx']),
        getTester(Or, ['dolor--sitAMET', 'worldx']),
        getTester(Or, ['xyz', 'ello woxx']),
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

    describe('similarity operations (LIKE)', () => {

      const values = Map<string, string>({
        Title: 'SitAmetus',
        age: '234',
        description: 'hello universe',
        first_name: 'Loremus',
        last_name: 'IpsuMus',
      });

      const matchingTesters = [
        getTester(Like, ['age', '234']),
        getTester(Like, ['first_name', 'Mus']),
        getTester(Like, ['last_name', 'mus']),
        getTester(Like, ['description', 'ello uni']),
        getTester(Like, ['Title', 'SitAmetus']),
      ];

      const notMatchingTesters = [
        getTester(Like, ['age', '5']),
        getTester(Like, ['first_name', 'umus']),
        getTester(Like, ['last_name', 'emus']),
        getTester(Like, ['description', 'elo unix']),
        getTester(Like, ['title', 'sitametus']),
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

        expect(String(matchingTesters[3].test(values)))
          .to.equal('Just(Map { "description": Match "hello universe" { Map {' +
            ' "ello uni": OrderedSet { [1, 9] }' +
          ' } } })');

        expect(String(matchingTesters[4].test(values)))
          .to.equal('Just(Map { "Title": Match "SitAmetus" { Map {' +
            ' "sitametus": OrderedSet { [0, 9] }' +
          ' } } })');

      });
    });

  });

});
