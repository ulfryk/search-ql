/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { Map, OrderedSet } from 'immutable';

import { BinaryOperationExpression, Expression, Operator, TermExpression } from '../../ast';
import { And0, config, Like0, Or0 } from '../../testing/utils';
import { BinaryOperationExpressionTester, Tester } from '../index';

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
        getTester(And0, ['All', 'good']),
        getTester(And0, ['asdffa', 'SDFAS']),
        getTester(And0, ['AND', 'OR']),
        getTester(Or0, ['dolor_sitAMET', 'world']),
        getTester(Or0, ['xyz', 'ello wo']),
      ];

      const notMatchingTesters = [
        getTester(And0, ['All', 'is good']),
        getTester(And0, ['asdffa', 'SDFASx']),
        getTester(And0, ['AND', 'OOxx']),
        getTester(Or0, ['dolor--sitAMET', 'worldx']),
        getTester(Or0, ['xyz', 'ello woxx']),
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
        getTester(Like0, ['age', '234']),
        getTester(Like0, ['first_name', 'Mus']),
        getTester(Like0, ['last_name', 'mus']),
        getTester(Like0, ['description', 'ello uni']),
        getTester(Like0, ['Title', 'SitAmetus']),
      ];

      const notMatchingTesters = [
        getTester(Like0, ['age', '5']),
        getTester(Like0, ['first_name', 'umus']),
        getTester(Like0, ['last_name', 'emus']),
        getTester(Like0, ['description', 'elo unix']),
        getTester(Like0, ['title', 'sitametus']),
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
