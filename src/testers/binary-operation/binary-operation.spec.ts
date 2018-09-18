/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { Map, OrderedSet } from 'immutable';

import { BinaryOperationExpression, fromMatch, Operator } from '../../ast';
import { Expression, ValueType } from '../../common/model';
import { ParserConfig } from '../../config';
import { And0, Gt0, Gte0, Is0, IsNot0, Like0, Lt0, Lte0, NotLike0, Or0 } from '../../testing/utils';
import { BinaryOperationExpressionTester, Tester } from '../index';

const getTester = (operator: Operator, values: [string, string], model: Map<string, ValueType>) => {
  const config = ParserConfig.create({ model });
  const [lhs, rhs] = values.map(fromMatch(config));
  const expr = BinaryOperationExpression.fromPair(operator)(lhs as any, rhs as any);

  return new BinaryOperationExpressionTester<any, any, any>(
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
        '',
      ].map((val, j) => [`label ${j}`, val.toLowerCase()])) as Map<string, string>;

      const model = values.map(() => ValueType.Text).toMap();

      const matchingTesters = [
        getTester(And0, ['All', 'good'], model),
        getTester(And0, ['asdffa', 'SDFAS'], model),
        getTester(And0, ['AND', 'OR'], model),
        getTester(Or0, ['dolor_sitAMET', 'world'], model),
        getTester(Or0, ['xyz', 'ello wo'], model),
      ];

      const notMatchingTesters = [
        getTester(And0, ['All', 'is good'], model),
        getTester(And0, ['asdffa', 'SDFASx'], model),
        getTester(And0, ['AND', 'OOxx'], model),
        getTester(Or0, ['dolor--sitAMET', 'worldx'], model),
        getTester(Or0, ['xyz', 'ello woxx'], model),
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

      it('should build proper Match output', () => {
        expect(String(matchingTesters[1].test(values).matches()))
          .to.equal('Just(Map { "label 1": Match "asdffa sdfas sdf" { Map {' +
          ' "asdffa": OrderedSet { [0, 6] },' +
          ' "sdfas": OrderedSet { [7, 12] }' +
          ' } } })');

        expect(String(matchingTesters[3].test(values).matches()))
          .to.equal('Just(Map { "label 4": Match "ipsum-dolor_sitamet" { Map {' +
          ' "dolor_sitamet": OrderedSet { [6, 19] }' +
          ' } } })');

        expect(String(matchingTesters[4].test(values).matches()))
          .to.equal('Just(Map { "label 5": Match "hello world" { Map {' +
          ' "ello wo": OrderedSet { [1, 8] }' +
          ' } } })');

      });

    });

    describe('similarity operations (LIKE, NOT_LIKE)', () => {

      const values = Map<string, string>({
        Title: 'SitAmetus',
        age: '234',
        description: 'hello universe',
        first_name: 'Loremus',
        last_name: 'IpsuMus',
      });

      const model = Map<string, ValueType>({
        Title: ValueType.Text,
        age: ValueType.Number,
        description: ValueType.Text,
        first_name: ValueType.Text,
        last_name: ValueType.Text,
      });

      const matchingTesters = [
        getTester(Like0, ['age', '234'], model),
        getTester(Like0, ['first_name', 'Mus'], model),
        getTester(Like0, ['last_name', 'mus'], model),
        getTester(Like0, ['description', 'ello uni'], model),
        getTester(Like0, ['Title', 'SitAmetus'], model),
        getTester(NotLike0, ['age', '233'], model),
        getTester(NotLike0, ['first_name', 'Muus'], model),
        getTester(NotLike0, ['last_name', 'muus'], model),
        getTester(NotLike0, ['description', 'ello unix'], model),
        getTester(NotLike0, ['Title', 'SiAmetus'], model),
      ];

      const trueTesters = [
        getTester(Like0, ['00234', '234.00'], model),
        getTester(Like0, ['sitametus', 'tamet'], model),
        getTester(NotLike0, ['234.01', '234.011'], model),
        getTester(NotLike0, ['sitametus', 'tametuss'], model),
      ];

      const falseTesters = [
        getTester(Like0, ['231', '234'], model),
        getTester(Like0, ['title', 'total'], model),
        getTester(NotLike0, ['00234', '234.00'], model),
        getTester(NotLike0, ['sitametus', 'tamet'], model),
      ];

      const notMatchingTesters = [
        getTester(Like0, ['age', '5'], model),
        getTester(Like0, ['first_name', 'umus'], model),
        getTester(Like0, ['last_name', 'emus'], model),
        getTester(Like0, ['description', 'elo unix'], model),
        getTester(Like0, ['Title', 'sitametuss'], model),
        getTester(NotLike0, ['age', '234'], model),
        getTester(NotLike0, ['first_name', 'Mus'], model),
        getTester(NotLike0, ['last_name', 'mus'], model),
        getTester(NotLike0, ['description', 'ello uni'], model),
        getTester(NotLike0, ['Title', 'SitAmetus'], model),
      ];

      matchingTesters.forEach(tester => {
        it(`should find expression "${tester.ast}"`, () => {
          expect(tester.test(values).type).to.equal(ValueType.Phrase);
          expect(tester.test(values).value).to.be.true;
          expect(tester.test(values).matches().isSome()).to.be.true;
        });
      });

      trueTesters.forEach(tester => {
        it(`should evaluate expression "${tester.ast}" to True`, () => {
          expect(tester.test(values).type).to.equal(ValueType.Boolean);
          expect(tester.test(values).value).to.be.true;
          expect(tester.test(values).matches().isNone()).to.be.true;
        });
      });

      falseTesters.forEach(tester => {
        it(`should evaluate expression "${tester.ast}" to False`, () => {
          expect(tester.test(values).type).to.equal(ValueType.Boolean);
          expect(tester.test(values).value).to.be.false;
          expect(tester.test(values).matches().isNone()).to.be.true;
        });
      });

      notMatchingTesters.forEach(tester => {
        it(`should not find expression "${tester.ast}"`, () => {
          expect(tester.test(values).type).to.equal(ValueType.Phrase);
          expect(tester.test(values).matches().isSome()).to.be.false;
        });
      });

      it('should build proper Match output', () => {

        expect(String(matchingTesters[3].test(values).matches()))
          .to.equal('Just(Map { "description": Match "hello universe" { Map {' +
            ' "ello uni": OrderedSet { [1, 9] }' +
          ' } } })');

        expect(String(matchingTesters[4].test(values).matches()))
          .to.equal('Just(Map { "Title": Match "SitAmetus" { Map {' +
            ' "sitametus": OrderedSet { [0, 9] }' +
          ' } } })');

      });
    });

    describe('equality operations (IS, IS_NOT)', () => {

      const values = Map<string, string>({
        Title: 'SitAmetus',
        age: '234',
        description: 'hello universe',
        first_name: 'Loremus',
        last_name: 'IpsuMus',
      });

      const model = Map<string, ValueType>({
        Title: ValueType.Text,
        age: ValueType.Number,
        description: ValueType.Text,
        first_name: ValueType.Text,
        last_name: ValueType.Text,
      });

      const matchingTesters = [
        getTester(Is0, ['age', '234'], model),
        getTester(Is0, ['first_name', 'loremus'], model),
        getTester(Is0, ['last_name', 'ipsumus'], model),
        getTester(Is0, ['description', 'hello universe'], model),
        getTester(Is0, ['Title', 'SitAmetus'], model),
        getTester(IsNot0, ['age', '5'], model),
        getTester(IsNot0, ['first_name', 'umus'], model),
        getTester(IsNot0, ['last_name', 'emus'], model),
        getTester(IsNot0, ['description', 'elo unix'], model),
        getTester(IsNot0, ['Title', 'sitametuss'], model),
      ];

      const notMatchingTesters = [
        getTester(IsNot0, ['age', '234'], model),
        getTester(IsNot0, ['first_name', 'Loremus'], model),
        getTester(IsNot0, ['last_name', 'IpsuMus'], model),
        getTester(IsNot0, ['description', 'hello universe'], model),
        getTester(IsNot0, ['Title', 'SitAmetus'], model),
        getTester(Is0, ['age', '5'], model),
        getTester(Is0, ['first_name', 'umus'], model),
        getTester(Is0, ['last_name', 'emus'], model),
        getTester(Is0, ['description', 'elo unix'], model),
        getTester(Is0, ['title', 'sitametus'], model),
        getTester(Is0, ['first_name', 'Mus'], model),
        getTester(Is0, ['last_name', 'mus'], model),
        getTester(Is0, ['description', 'ello uni'], model),
        getTester(Is0, ['Title', 'SitAmet'], model),
      ];

      const trueTesters = [
        getTester(Is0, ['234', '234'], model),
        getTester(IsNot0, ['title', 'sitametus'], model),
      ];

      const falseTesters = [
        getTester(Is0, ['231', '234'], model),
        getTester(IsNot0, ['title', 'title'], model),
      ];

      matchingTesters.forEach(tester => {
        it(`should find expression "${tester.ast}"`, () => {
          expect(tester.test(values).value).to.be.true;
          expect(tester.test(values).matches().isSome()).to.be.true;
        });
      });

      trueTesters.forEach(tester => {
        it(`should evaluate expression "${tester.ast}" to True`, () => {
          expect(tester.test(values).value).to.be.true;
          expect(tester.test(values).matches().isNone()).to.be.true;
        });
      });

      falseTesters.forEach(tester => {
        it(`should evaluate expression "${tester.ast}" to False`, () => {
          expect(tester.test(values).value).to.be.false;
          expect(tester.test(values).matches().isNone()).to.be.true;
        });
      });

      notMatchingTesters.forEach(tester => {
        it(`should not find expression "${tester.ast}"`, () => {
          expect(tester.test(values).value).to.be.false;
          expect(tester.test(values).matches().isSome()).to.be.false;
        });
      });

      it('should build proper Match output', () => {

        expect(String(matchingTesters[3].test(values).matches()))
          .to.equal('Just(Map { "description": Match "hello universe" { Map {' +
            ' "hello universe": OrderedSet { [0, 14] }' +
          ' } } })');

        expect(String(matchingTesters[4].test(values).matches()))
          .to.equal('Just(Map { "Title": Match "SitAmetus" { Map {' +
            ' "sitametus": OrderedSet { [0, 9] }' +
          ' } } })');

        expect(String(matchingTesters[8].test(values).matches()))
          .to.equal('Just(Map {})');

      });
    });

    describe('relation operations (GT, GTE, LT, LTE)', () => {

      const values = Map<string, string>({
        date: '2018-01-01',
        gross: '123',
        name: 'an item',
        net: '100',
        tax: '23',
      });

      const model = Map<string, ValueType>({
        date: ValueType.Date,
        gross: ValueType.Number,
        name: ValueType.Text,
        net: ValueType.Number,
        tax: ValueType.Number,
      });

      const trueTesters = [
        getTester(Gt0, ['gross', '100'], model),
        getTester(Gt0, ['name', 'aa item'], model),
        getTester(Gt0, ['date', '2017-05-21'], model),

        getTester(Gte0, ['gross', '100'], model),
        getTester(Gte0, ['net', '100'], model),
        getTester(Gte0, ['date', '2018-01-01'], model),
        getTester(Gte0, ['date', '2017-05-21'], model),

        getTester(Lt0, ['gross', '125'], model),
        getTester(Lt0, ['tax', '25'], model),
        getTester(Lt0, ['date', '2018-05-21'], model),

        getTester(Lte0, ['date', '2018-01-01'], model),
        getTester(Lte0, ['date', '2018-05-21'], model),
        getTester(Lte0, ['name', 'an item'], model),
      ];

      const falseTesters = [
        getTester(Lte0, ['gross', '100'], model),
        getTester(Lte0, ['net', '99'], model),
        getTester(Lte0, ['date', '2017-05-21'], model),
        getTester(Lte0, ['name', 'aa item'], model),
      ];

      trueTesters.forEach(tester => {
        it(`should evaluate expression "${tester.ast}" to True`, () => {
          expect(tester.test(values).value).to.be.true;
          expect(tester.test(values).matches().isNone()).to.be.true;
        });
      });

      falseTesters.forEach(tester => {
        it(`should evaluate expression "${tester.ast}" to False`, () => {
          expect(tester.test(values).value).to.be.false;
          expect(tester.test(values).matches().isNone()).to.be.true;
        });
      });
    });

  });

});
