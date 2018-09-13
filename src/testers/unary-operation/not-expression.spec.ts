/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { Map } from 'immutable';

import { fromMatch, NotExpression, PhraseExpression } from '../../ast';
import { ParserConfig } from '../../config';
import { PhraseExpressionTester } from '../term';
import { NotExpressionTester } from './not-expression';

const config = new ParserConfig();

const getTester = (value: string): NotExpressionTester => {
  const text = PhraseExpression.fromTerm(fromMatch(config)(value));
  const expr = new NotExpression(text);

  return new NotExpressionTester(expr, new PhraseExpressionTester(text, config), config);
};

describe('SearchQL testers', () => {

  describe('NotExpressionTester', () => {

    // tslint:disable-next-line:no-unnecessary-type-assertion
    const values = Map([
      'All good',
      'asdffa SDFAS sdf',
      ')((',
      'AND OR OR AND',
      'IpsUM-dolor_sitAMET',
      'hello world',
    ].map((val, i) => [`label ${i}`, val.toLowerCase()])) as Map<string, string>;

    const notMatchingTesters = values.toArray()
      .map((val: string) => val.substr(-6, 5))
      .concat(values.toArray())
      .map(getTester);

    const matchingTesters = values.toArray()
      .map((val, i) => `${i} ${val}`)
      .map(getTester);

    matchingTesters.forEach(tester => {
      it(`should find expression "${tester.ast}"`, () => {
        expect(tester.test(values).matches().isSome()).to.be.true;
      });
    });

    notMatchingTesters.forEach(tester => {
      it(`should not find expression "${tester.ast}"`, () => {
        expect(tester.test(values).matches().isSome()).to.be.false;
      });
    });

    it('should not find expression "zzz" in few fields', () => {
      expect(
        getTester('zzz').test(Map({
          one: 'aaa bbb aaa aaa aaasda ddaaa',
          three: 'aaGaa bbb aadaaXaadaa ddddadd',
          two: 'aaa bbb aaaXaaa',
        })).matches().toString(),
      ).to.equal('Just(Map {' +
        ' "one": Match "aaa bbb aaa aaa aaasda ddaaa" { Map {} },' +
        ' "three": Match "aaGaa bbb aadaaXaadaa ddddadd" { Map {} },' +
        ' "two": Match "aaa bbb aaaXaaa" { Map {} }' +
        ' })');
    });

  });

});
