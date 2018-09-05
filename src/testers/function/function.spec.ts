/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { Map } from 'immutable';

import { Expression, ValueType } from '../../common/model';
import { config, fn, txt } from '../../testing/utils';
import { FunctionExpressionTester, Tester } from '../index';

const getTester = (name: string, ...args: Expression[]) => {
  const expr = fn(name, ValueType.Boolean)(...args);

  return new FunctionExpressionTester(
    expr,
    expr.value.map(Tester.fromAst(config)).toList(),
    config);
};

describe('SearchQL testers', () => {

  describe('FunctionExpressionTester', () => {

    describe('built in test_function', () => {

      // tslint:disable-next-line:no-unnecessary-type-assertion
      const values = Map([
        'All good',
        'test_function(aaa, bbb)',
        'test_function()',
        'AND OR OR AND',
        'IpsUM-dolor_sitAMET',
        'hello world',
      ].map((val, j) => [`label ${j}`, val.toLowerCase()])) as Map<string, string>;

      const matchingTesters = [] as FunctionExpressionTester<any>[];

      const notMatchingTesters = [
        getTester('test_function'),
        getTester('test_function', txt('aaa'), txt('bbb')),
      ];

      matchingTesters.forEach(tester => {
        it(`should find expression "${tester.ast}"`, () => {
          expect(tester.test(values).matches().isSome()).to.be.true;
          expect(tester.test(values).matches().some().isEmpty()).to.be.false;
        });
      });

      notMatchingTesters.forEach(tester => {
        it(`should not find expression "${tester.ast}"`, () => {
          expect(tester.test(values).matches().isSome()).to.be.false;
        });
      });

    });

  });

});
