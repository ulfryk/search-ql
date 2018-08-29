/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import { zip } from 'lodash';

import { Expression } from '../ast';
import { and, And0, andNot, config, like, Like0, not, or, Or0, txt } from '../testing/utils';
import { ParserName } from './names';
import { QueryParserFactory } from './query-parser-factory';

const { AND, EXACT_MATCHER, GROUP_END, GROUP_START, OR } = config;

const test = (
  validInput: string[],
  validOutput: Expression[],
  invalidInput: string[],
  parserNames: ParserName[],
) => {
  zip<any>(validInput, validOutput).forEach(([input, output]) => {
    describe(`for valid input: '${input}'`, () => {
      const parsed = new QueryParserFactory(parserNames).getParser().parse(input);

      it('should succeed', () => {
        expect(parsed.status).to.be.true;
      });

      it('should provide proper value', () => {
        expect(parsed.status && parsed.value.equals(output)).to.be.true;
        expect(parsed.status ? String(parsed.value) : null).to.equal(String(output));
      });

    });
  });

  invalidInput.forEach(input => {
    describe(`for invalid input: '${input}'`, () => {

      it('should fail', () => {
        expect(new QueryParserFactory(parserNames).getParser().parse(input).status).to.be.false;
      });

    });
  });
};

describe('SearchQL parsers', () => {

  describe('QueryParserFactory', () => {

    describe('with all parsers', () => {

      const allParserNames =
        [ParserName.Basic, ParserName.BinaryOperation, ParserName.Not];

      const validInput = [
        'aa',
        `a${And0.token}`,
        `(((${Or0.token}a)))`,
        '"aa:bb AND cc dd"',
        'aa LIKE AA',
        'aa AND bb',
        '(aa & bb)',
        'aa AND (bb LIKE BB OR cc) | (dd AND (ee LIKE EE OR ff))',
        'aaa AND bbb OR ccc AND ddd',
        'NOT abc',
        'aa AND NOT bb',
        'aa NOT bb',
        'NOT aaa AND NOT bbb',
        'NOT (aaa OR bbb) AND ccc',
        'NOT aaa AND bb LIKE BB', //
        'aaa AND (bbb OR ccc) NOT ddd',
        '(aaa OR bbb) NOT ccc',
        'aaa AND bbb AND ccc NOT ddd',
      ];

      const validOutput = [
        txt('aa'),
        txt(`a${And0.token}`),
        txt(`${Or0.token}a`),
        txt('aa:bb AND cc dd'),
        like(txt('aa'), txt('AA'), Like0),
        and(txt('aa'), txt('bb'), And0),
        and(txt('aa'), txt('bb')),
        or(
          and(txt('aa'), or(like(txt('bb'), txt('BB'), Like0), txt('cc'), Or0), And0),
          and(txt('dd'), or(like(txt('ee'), txt('EE'), Like0), txt('ff'), Or0), And0)),
        or(and(txt('aaa'), txt('bbb'), And0), and(txt('ccc'), txt('ddd'), And0), Or0),
        not(txt('abc')),
        andNot(txt('aa'), txt('bb')),
        andNot(txt('aa'), txt('bb')),
        and(not(txt('aaa')), not(txt('bbb')), And0),
        and(not(or(txt('aaa'), txt('bbb'), Or0)), txt('ccc'), And0),
        and(not(txt('aaa')), like(txt('bb'), txt('BB'), Like0), And0),
        and(and(txt('aaa'), or(txt('bbb'), txt('ccc'), Or0), And0), not(txt('ddd')), And0),
        and(or(txt('aaa'), txt('bbb'), Or0), not(txt('ccc')), And0),
        and(and(and(txt('aaa'), txt('bbb'), And0), txt('ccc'), And0), not(txt('ddd')), And0),
      ];

      const invalidInput = [
        'ASDfas 32%@$%4512 u954anna as d][;];.{P} AND',
        'OR AND NOT (OR AND NOT) asd: asd not ASD:ASd',
        ' ',
        ...AND,
        ...OR,
        '123"asd',
        '"text',
        `${GROUP_START}asd`,
        `asd${GROUP_END}`,
        `dasd${EXACT_MATCHER}a`,
      ];

      test(validInput, validOutput, invalidInput, allParserNames);

    });

    describe('with some parsers excluded', () => {

      const subsetOfAllParsers = [ParserName.Basic, ParserName.BinaryOperation];

      const validInput = [
        'aa',
        'bb & cc',
      ];

      const validOutput = [
        txt('aa'),
        and(txt('bb'), txt('cc')),
      ];

      const invalidInput = [
        'desc abc AND',
        'NOT aaa',
        '! aaa',
      ];

      test(validInput, validOutput, invalidInput, subsetOfAllParsers);

    });

  });

});