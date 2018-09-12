/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import { zip } from 'lodash';

import { Expression } from '../common/model';
import { ParserConfig, ParserName } from '../config';
import { and, And0, andNot, config, fn, is, isNot, like, Like0, not, or, Or0, phrase, txt } from '../testing/utils';
import { QueryParserFactory } from './query-parser-factory';

const { AND, EXACT_MATCHER, GROUP_END, GROUP_START, OR } = config;

const allParserNames = [
  ParserName.Basic,
  ParserName.BinaryOperation,
  ParserName.Function,
  ParserName.Not,
];

const test = (
  validInput: string[],
  validOutput: Expression[],
  invalidInput: string[],
  parserNames: ParserName[],
) => {
  zip<any>(validInput, validOutput).forEach(([input, output]) => {
    describe(`for valid input: '${input}'`, () => {
      const parsed = new QueryParserFactory(ParserConfig.create({ parserNames }))
        .getParser().parse(input);

      it('should succeed', () => {
        expect(parsed.status).to.be.true;
      });

      // tslint:disable-next-line:cyclomatic-complexity
      it('should provide proper value', () => {
        expect(parsed.status && parsed.value.reshape().equals(output)).to.be.true;
        expect(parsed.status ? String(parsed.value.reshape()) : null).to.equal(String(output));
      });

    });
  });

  invalidInput.forEach(input => {
    describe(`for invalid input: '${input}'`, () => {

      it('should fail', () => {
        expect(new QueryParserFactory(ParserConfig.create({ parserNames }))
          .getParser().parse(input).status).to.be.false;
      });

    });
  });
};

describe('SearchQL parsers', () => {

  describe('QueryParserFactory', () => {

    describe('with all parsers', () => {

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
        'age != 16 & name = John',
        'age = 24 & name != Doe',
        'age != 16 & name = John | age = 24 & name != Doe',
      ];

      const validOutput = [
        phrase('aa'),
        phrase(`a${And0.token}`),
        phrase(`${Or0.token}a`),
        phrase('aa:bb AND cc dd'),
        like(phrase('aa'), phrase('AA'), Like0),
        and(phrase('aa'), phrase('bb'), And0),
        and(phrase('aa'), phrase('bb')),
        or(
          and(phrase('aa'), or(like(phrase('bb'), phrase('BB'), Like0), phrase('cc'), Or0), And0),
          and(phrase('dd'), or(like(phrase('ee'), phrase('EE'), Like0), phrase('ff'), Or0), And0)),
        or(and(phrase('aaa'), phrase('bbb'), And0), and(phrase('ccc'), phrase('ddd'), And0), Or0),
        not(phrase('abc')),
        andNot(phrase('aa'), phrase('bb')),
        andNot(phrase('aa'), phrase('bb')),
        and(not(phrase('aaa')), not(phrase('bbb')), And0),
        and(not(or(phrase('aaa'), phrase('bbb'), Or0)), phrase('ccc'), And0),
        and(not(phrase('aaa')), like(phrase('bb'), phrase('BB'), Like0), And0),
        and(and(phrase('aaa'), or(phrase('bbb'), phrase('ccc'), Or0), And0), not(phrase('ddd')), And0),
        and(or(phrase('aaa'), phrase('bbb'), Or0), not(phrase('ccc')), And0),
        and(and(and(phrase('aaa'), phrase('bbb'), And0), phrase('ccc'), And0), not(phrase('ddd')), And0),
        and(isNot(txt('age'), phrase('16')), is(txt('name'), phrase('John'))),
        and(is(txt('age'), phrase('24')), isNot(txt('name'), phrase('Doe'))),
        or(
          and(isNot(txt('age'), phrase('16')), is(txt('name'), phrase('John'))),
          and(is(txt('age'), phrase('24')), isNot(txt('name'), phrase('Doe')))),
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

      // TODO: Used parsers should have impact on restricted signs in SyntaxConfig

      [

        {
          invalidInput: ['test_function(aaa)', 'desc AND a', 'NOT aaa', 'z | aaa', '$#%564 *(&^@%#1 1~`1`'],
          parsers: [ParserName.Basic],
          validInput: ['aa', '"$#%564 *(&^@%#1 1~`1`"'],
          validOutput: [phrase('aa'), phrase('$#%564 *(&^@%#1 1~`1`')],
        },

        {
          invalidInput: ['test_function(aaa)', 'desc abc AND', 'NOT aaa', '! aaa'],
          parsers: [ParserName.Basic, ParserName.BinaryOperation],
          validInput: ['aa', 'bb & cc'],
          validOutput: [phrase('aa'), and(phrase('bb'), phrase('cc'))],
        },

        {
          invalidInput: ['desc AND asd', 'NOT aaa', '! aaa'],
          parsers: [ParserName.Basic, ParserName.Function],
          validInput: [
            'test_function(aaa)',
            'test_function(aaa, test_function(aaa, bbb), 12)',
          ],
          validOutput: [
            fn('test_function')(txt('aaa')),
            fn('test_function')(txt('aaa'), fn('test_function')(txt('aaa'), txt('bbb')), txt('12')),
          ],
        },

        {
          invalidInput: ['asd! !!asd', 'c NOT a', 'c & a'],
          parsers: [ParserName.Basic, ParserName.Not],
          validInput: ['! a', 'NOT a'],
          validOutput: [not(phrase('a')), not(phrase('a'))],
        },

        {
          invalidInput: ['abc'],
          parsers: [ParserName.Function, ParserName.Not, ParserName.BinaryOperation],
          validInput: [
            '! test_function()',
            'test_function() & test_function() | test_function()',
            'test_function() ! test_function(NOT test_function())',
          ],
          validOutput: [
            not(fn('test_function')()),
            or(and(fn('test_function')(), fn('test_function')()), fn('test_function')()),
            andNot(fn('test_function')(), fn('test_function')(not(fn('test_function')()))),
          ],
        },

      ].forEach(({ invalidInput, parsers, validInput, validOutput }) => {
        describe(`\n      - using only ${parsers.join(', ')} parsers`, () => {
          test(validInput, validOutput, invalidInput, parsers);
        });
      });

    });

  });

});
