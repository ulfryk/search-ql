/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { List, Map } from 'immutable';

import { Expression, FunctionExpression } from '../../index';
import { config, txt } from '../../testing/utils';

import { TesterConfig } from '../config';
import { FunctionExpressionTester, Tester } from '../index';

const tConfig = TesterConfig.create({});

const getFnTester = (name: string) => (...args: Expression[]) =>
  new FunctionExpressionTester<any>(
    FunctionExpression.fromParseResult(config.functions.get(name), args),
    List(args.map(Tester.fromAst(tConfig))),
    tConfig);

describe('SearchQL testers', () => {

  describe('FunctionExpressionTester', () => {

    describe('built in test_function', () => {

      const values = Map({
        age: '45',
        bio: 'Lorem ipsum dolor sit amet',
        email: 'john.doe@example.com',
        firstName: 'John',
        lastName: 'Doe',
        shortBio: '',
      });

      const matchingTesters = [
        getFnTester('is_empty')(txt('shortBio')),
      ];

      const notMatchingTesters = [
        getFnTester('is_empty')(txt('email')),
        getFnTester('test_function')(),
        getFnTester('test_function')(txt('aaa'), txt('bbb')),
      ];

      matchingTesters.forEach(tester => {
        const output = tester.test(values);
        const match = output.matches();
        it(`should find expression "${tester.ast}"`, () => {
          expect(output.value, String(output)).to.be.true;
          expect(match.some().isEmpty(), String(match)).to.be.false;
        });
      });

      notMatchingTesters.forEach(tester => {
        const output = tester.test(values);
        const match = output.matches();
        it(`should not find expression "${tester.ast}"`, () => {
          expect(output.value, String(output)).to.be.false;
          expect(match.isSome(), String(match)).to.be.false;
        });
      });

    });

  });

});
