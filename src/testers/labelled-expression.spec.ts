/* tslint:disable:no-unused-expression no-magic-numbers no-import-side-effect  */
import '@samwise-tech/immutable';
import { expect } from 'chai';
import { Map } from 'immutable';
import * as _ from 'lodash';

import { BasicExpression, LabelledExpression } from '../expressions';
import { SyntaxConfig } from '../syntax-config';
import { BasicExpressionTester } from './basic-expression';
import { LabelledExpressionTester } from './labelled-expression';

const config = new SyntaxConfig();

const getTester = (label: string, value: string) => {
  const expr = new LabelledExpression(label, new BasicExpression(value));

  return new LabelledExpressionTester(expr, new BasicExpressionTester(expr.value, config), config);
};

describe('SearchQL testers', () => {

  describe('LabelledExpressionTester', () => {

    // tslint:disable-next-line:no-unnecessary-type-assertion
    const values = Map([
      'All good',
      'asdffa SDFAS sdf',
      ')((',
      'AND OR OR AND',
      'IpsUM-dolor_sitAMET',
      'hello world',
    ].map((val, i) => [`label ${i}`, val.toLowerCase()])) as Map<string, string>;

    const matchingTesters = values
      .map((val: string) => val.substr(-6, 5))
      .entrySeq()
      .concat(values.entrySeq())
      .toArray()
      .map(([key, value]) => getTester(key, value));

    const notMatchingTesters = values
      .map(val => `${val} additional text`)
      .entrySeq()
      .toArray()
      .map(([key, value]) => getTester(key, value));

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

  });

});
