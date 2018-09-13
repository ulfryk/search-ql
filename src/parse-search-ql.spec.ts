/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import { Map } from 'immutable';
import { zip } from 'lodash';
import { Either } from 'monet';

import { AndOperator, LikeOperator, OrOperator } from './ast';
import { ParseFailure, ValueType } from './common/model';
import { ParserName } from './config';
import { parseSearchQL } from './parse-search-ql';
import { and, fn, isNotR, isR, like, likeR, or, phrase, txt } from './testing/utils';

const allParserNames = [
  ParserName.Basic,
  ParserName.BinaryOperation,
  ParserName.Function,
  ParserName.Not,
];

describe('SearchQL', () => {

  describe('parseSearchQL with regular config', () => {

    const validInput = [
      'aaa & bbb',
      'token_expired ~ true',
      'first_name ~ Adam | token_expired ~ true',
      'test_function(aaa, "b(b & b)")',
      'aaa = bbb & cc != dd',
    ];

    const successfulOutputValues = [
      and(phrase('aaa'), phrase('bbb')),
      likeR(txt('token_expired'), txt('true')),
      or(likeR(txt('first_name'), txt('Adam')), likeR(txt('token_expired'), txt('true'))),
      fn('test_function')(txt('aaa'), txt('b(b & b)')),
      and(isR(txt('aaa'), txt('bbb')), isNotR(txt('cc'), txt('dd'))),
    ].map(Either.of);

    const invalidInput = [
      'ASDfas 32%@$%4512 u954anna as d][;];.{P} AND',
      'asdasd OR AND NOT (OR AND NOT) asd: asd not ASD:ASd',
      ' ',
      'des:: c: ipsum OR NOT dolor AND "john doe":"',
    ];

    const invalidTypesInput = [
      'test_function(aaa, (token_expired ~ true))',
      'test_function(test_function(aaa), (token_expired ~ true), (! is_empty(ccc)))',
      'test_function(aaa, bbb, (! ccc)) ~ (! "John Doe")',
      'a ISNT is_empty(b)',
    ];

    const invalidTypesOutput = [
      ['TypeError: Function "test_function" has wrong 2nd rest arg passed, should be TEXT but is BOOLEAN'],
      [
        'TypeError: Function "test_function" has wrong 1st rest arg passed, should be TEXT but is BOOLEAN',
        'TypeError: Function "test_function" has wrong 2nd rest arg passed, should be TEXT but is BOOLEAN',
        'TypeError: Function "test_function" has wrong 3rd rest arg passed, should be TEXT but is BOOLEAN',
      ],
      [
        'TypeError: Function \"test_function\" has wrong 3rd rest arg passed, should be TEXT but is PHRASE',
      ],
      [
        'TypeError: Both sides of ISNT expression should be of same type, but got LHS: TEXT and RHS: BOOLEAN.',
      ],
    ];

    zip<any>(validInput, successfulOutputValues).forEach(([input, output]) => {
      describe(`for valid input: ${input}`, () => {
        const parsed = parseSearchQL({ parserNames: allParserNames })(input);

        it('should return Right', () => {
          expect(parsed.isRight(), parsed.cata(String, String)).to.be.true;
        });

        it('should return parsed expression', () => {
          expect(parsed.equals(output)).to.be.true;
        });

      });
    });

    invalidInput.forEach(input => {
      describe(`for invalid input: ${input}`, () => {
        const parsed = parseSearchQL({ parserNames: allParserNames })(input);

        it('should return Left', () => {
          expect(parsed.isLeft()).to.be.true;
        });

        it('should provide original input', () => {
          expect(parsed.cata(
            f => f.map(({ query }: ParseFailure) => query)[0],
            () => null)).to.equal(input);
        });

      });
    });

    invalidTypesInput.forEach((input, i) => {
      describe(`for syntactically valid yet wrongly typed input: ${input}`, () => {
        const parsed = parseSearchQL({ parserNames: allParserNames })(input);

        it('should return Left', () => {
          expect(parsed.isLeft(), parsed.cata(String, String)).to.be.true;
        });

        it('should yield proper type error information', () => {
          expect(parsed.left().map(String)).to.deep.equal(invalidTypesOutput[i]);
        });

      });
    });

  });

  describe('parseSearchQL with regular config and defined model', () => {

    const model = Map<string, ValueType>({
      first_name: ValueType.Text,
      token_expired: ValueType.Text, // add BooleanTerm
    });

    const validInput = [
      'aaa & bbb',
      'token_expired ~ true',
      'first_name ~ Adam | token_expired ~ true',
      'test_function(aaa, "b(b & b)")',
      'aaa = bbb & cc != dd',
    ];

    const successfulOutputValues = [
      and(phrase('aaa'), phrase('bbb')),
      like(txt('token_expired'), txt('true')),
      or(like(txt('first_name'), txt('Adam')), like(txt('token_expired'), txt('true'))),
      fn('test_function')(txt('aaa'), txt('b(b & b)')),
      and(isR(txt('aaa'), txt('bbb')), isNotR(txt('cc'), txt('dd'))),
    ].map(Either.of);

    const invalidInput = [
      'ASDfas 32%@$%4512 u954anna as d][;];.{P} AND',
      'asdasd OR AND NOT (OR AND NOT) asd: asd not ASD:ASd',
      ' ',
      'des:: c: ipsum OR NOT dolor AND "john doe":"',
    ];

    const invalidTypesInput = [
      'test_function(aaa, (token_expired ~ true))',
      'test_function(test_function(aaa), (token_expired ~ true), (! is_empty(ccc)))',
      'test_function(aaa, bbb, (! ccc)) ~ (! (first_name IS "John Doe"))',
      'a ISNT is_empty(b)',
    ];

    const invalidTypesOutput = [
      ['TypeError: Function "test_function" has wrong 2nd rest arg passed, should be TEXT but is BOOLEAN'],
      [
        'TypeError: Function "test_function" has wrong 1st rest arg passed, should be TEXT but is BOOLEAN',
        'TypeError: Function "test_function" has wrong 2nd rest arg passed, should be TEXT but is BOOLEAN',
        'TypeError: Function "test_function" has wrong 3rd rest arg passed, should be TEXT but is BOOLEAN',
      ],
      [
        'TypeError: Function \"test_function\" has wrong 3rd rest arg passed, should be TEXT but is PHRASE',
      ],
      [
        'TypeError: Both sides of ISNT expression should be of same type, but got LHS: TEXT and RHS: BOOLEAN.',
      ],
    ];

    zip<any>(validInput, successfulOutputValues).forEach(([input, output]) => {
      describe(`for valid input: ${input}`, () => {
        const parsed = parseSearchQL({ model, parserNames: allParserNames })(input);

        it('should return Right', () => {
          expect(parsed.isRight(), parsed.cata(String, String)).to.be.true;
        });

        it('should return parsed expression', () => {
          expect(parsed.equals(output)).to.be.true;
        });

      });
    });

    invalidInput.forEach(input => {
      describe(`for invalid input: ${input}`, () => {
        const parsed = parseSearchQL({ model, parserNames: allParserNames })(input);

        it('should return Left', () => {
          expect(parsed.isLeft()).to.be.true;
        });

        it('should provide original input', () => {
          expect(parsed.cata(
            f => f.map(({ query }: ParseFailure) => query)[0],
            () => null)).to.equal(input);
        });

      });
    });

    invalidTypesInput.forEach((input, i) => {
      describe(`for syntactically valid yet wrongly typed input: ${input}`, () => {
        const parsed = parseSearchQL({ parserNames: allParserNames })(input);

        it('should return Left', () => {
          expect(parsed.isLeft(), parsed.cata(String, String)).to.be.true;
        });

        it('should yield proper type error information', () => {
          expect(parsed.left().map(String)).to.deep.equal(invalidTypesOutput[i]);
        });

      });
    });

  });

  describe('parseSearchQL with custom config', () => {

    const model = Map<string, ValueType>({
      first_name: ValueType.Text,
    });

    const configC = {
      AND: ['&&'],
      FN_ARG_SEPARATOR: '|',
      FN_LEFT_PAREN: '/',
      FN_RIGHT_PAREN: '/',
      LIKE: ['~='],
      OR: ['||'],
      model,
      parserNames: allParserNames,
    };

    const AndC = new AndOperator('&&');
    const LikeC = new LikeOperator('||');
    const OrC = new OrOperator('~=');

    const validInput = [
      'aaa && bbb',
      '(aaa || bbb)',
      'first_name ~= Adam',
      'first_name ~= Adam && token_expired ~= true',
      'test_function/    aaa|bbb/',
    ];

    const successfulOutputValues = [
      and(phrase('aaa'), phrase('bbb'), AndC),
      or(phrase('aaa'), phrase('bbb'), OrC),
      like(txt('first_name'), txt('Adam'), LikeC),
      and(
        like(txt('first_name'), txt('Adam'), LikeC),
        likeR(txt('token_expired'), txt('true'), LikeC),
        AndC),
      fn('test_function')(txt('aaa'), txt('bbb')),
    ].map(Either.of);

    const invalidInput = [
      'aa&aa',
      'aa|aa',
      'aa~aa',
      'aa=aa',
      'aa/aa',
      'ASDfas 32%@$%4512 u954anna as d][;];.{P} &&',
      'asdasd || && NOT (|| && NOT) asd~ asd not ASD~ASd',
      ' ',
      'des~~ c~ ipsum || NOT dolor && "john doe"~"',
      'test_function/aaa|bbb\\',
    ];

    zip<any>(validInput, successfulOutputValues).forEach(([input, output]) => {
      describe(`for valid input: ${input}`, () => {
        const parsed = parseSearchQL(configC)(input);

        it('should return Right', () => {
          expect(parsed.isRight(), parsed.cata(String, String)).to.be.true;
        });

        it('should return parsed expression', () => {
          expect(parsed.equals(output)).to.be.true;
        });

      });
    });

    invalidInput.forEach(input => {
      describe(`for invalid input: ${input}`, () => {
        const parsed = parseSearchQL(configC)(input);

        it('should return Left', () => {
          expect(parsed.isLeft()).to.be.true;
        });

        it('should provide original input', () => {
          expect(parsed.cata(
            f => f.map(({ query }: ParseFailure) => query)[0],
            () => null)).to.equal(input);
        });

      });
    });

  });

});
