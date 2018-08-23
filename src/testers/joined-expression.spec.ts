/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { Map, Set } from 'immutable';
import * as _ from 'lodash';

import { BasicExpression, JoinedExpression } from '../expressions';
import { SyntaxConfig } from '../syntax-config';
import { BasicExpressionTester } from './basic-expression';
import { JoinedExpressionTester } from './joined-expression';

const config = new SyntaxConfig();
const { AND, OR } = config;

const getTester = (operator: string, values: string[]) => {
  const expr = new JoinedExpression(operator, Set(values.map(BasicExpression.fromMatch)));

  return new JoinedExpressionTester(
    expr,
    expr.value
      .map((child: BasicExpression) => new BasicExpressionTester(child, config))
      .toOrderedSet(),
    config);
};

describe('SearchQL expressions', () => {

  describe('JoinedExpression', () => {

    describe('test() method', () => {

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
        getTester(AND, ['All', 'good']),
        getTester(AND, ['asdffa', 'SDFAS', 'sdf']),
        getTester(AND, ['AND', 'OR', 'AND']),
        getTester(OR, ['dolor_sitAMET', 'world']),
        getTester(OR, ['xyz', 'zyx', 'ello wo', '12-12']),
      ];

      const notMatchingTesters = [
        getTester(AND, ['All', 'is good']),
        getTester(AND, ['asdffa', 'SDFAS', 'sdfx']),
        getTester(AND, ['AND', 'OR', 'OOxx']),
        getTester(OR, ['dolor--sitAMET', 'worldx']),
        getTester(OR, ['xyz', 'zyx', 'ello woxx']),
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
          ' "sdf": OrderedSet { [1, 4], [7, 10], [13, 16] },' +
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

});
