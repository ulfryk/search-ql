/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import { Map } from 'immutable';
import { zip } from 'lodash';
import { Either } from 'monet';

import { AndOperator, LikeOperator, OrOperator } from './ast';
import { ParseFailure, ValueType } from './common/model';
import { SearchQLParser } from './search-ql-parser';
import { and, fn, func, gteR, gtR, isNotR, isR, like, likeR, lteR, ltR, not, notLike, num, or, phrase, sel, txt } from './testing/utils';

describe('SearchQLParser', () => {

  describe('with regular config', () => {

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
      '(bb AND cc) >= bbb',
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
      [
        'TypeError: Both sides of >= expression should be of same type, but got LHS: PHRASE and RHS: TEXT.',
      ],
    ];

    zip<any>(validInput, successfulOutputValues).forEach(([input, output]) => {
      describe(`for valid input: ${input}`, () => {
        const parsed = SearchQLParser.create().parse(input);

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
        const parsed = SearchQLParser.create().parse(input);

        it('should return Left', () => {
          expect(parsed.isLeft()).to.be.true;
        });

        it('should provide original input', () => {
          expect(parsed.cata(
            f => f.map(failure => (failure as ParseFailure).query)[0],
            () => null)).to.equal(input);
        });

      });
    });

    invalidTypesInput.forEach((input, i) => {
      describe(`for syntactically valid yet wrongly typed input: ${input}`, () => {
        const parsed = SearchQLParser.create().parse(input);

        it('should return Left', () => {
          expect(parsed.isLeft(), parsed.cata(String, String)).to.be.true;
        });

        it('should yield proper type error information', () => {
          expect(parsed.left().map(String)).to.deep.equal(invalidTypesOutput[i]);
        });

      });
    });

  });

  describe('with regular config and defined model', () => {

    const model = Map<string, ValueType>({
      age: ValueType.Number,
      first_name: ValueType.Text,
      token_expired: ValueType.Text, // add BooleanTerm
    });

    const validInput = [
      'aaa & bbb',
      'token_expired ~ true',
      'first_name ~ Adam | token_expired ~ true',
      'test_function(aaa, "b(b & b)")',
      'aaa = bbb & cc != dd',
      'is_empty(first_name)',
      '! is_empty(first_name)',
      'first_name !~ noone',
      'age >= 13 & age < 18 | length(first_name) > 1 & length(first_name) <= 4',
      'length(token_expired) >= age',
    ];

    const successfulOutputValues = [
      and(phrase('aaa'), phrase('bbb')),
      like(txt('token_expired'), txt('true')),
      or(like(txt('first_name'), txt('Adam')), like(txt('token_expired'), txt('true'))),
      fn('test_function')(txt('aaa'), txt('b(b & b)')),
      and(isR(txt('aaa'), txt('bbb')), isNotR(txt('cc'), txt('dd'))),
      func('is_empty')(txt('first_name')),
      not(func('is_empty')(txt('first_name'))),
      notLike(txt('first_name'), txt('noone')),
      or(
        and(
          gteR(sel('age', ValueType.Number), num('13')),
          ltR(sel('age', ValueType.Number), num('18'))),
        and(
          gtR(func('length')(txt('first_name')), num('1')),
          lteR(func('length')(txt('first_name')), num('4')))),
      gteR(func('length')(txt('token_expired')), sel('age', ValueType.Number)),
    ].map(Either.of);

    const invalidInput = [
      'ASDfas 32%@$%4512 u954anna as d][;];.{P} AND',
      'asdasd OR AND NOT (OR AND NOT) asd: asd not ASD:ASd',
      ' ',
      'des:: c: ipsum OR NOT dolor AND "john doe":"',
    ];

    const invalidTypesInput = [
      'test_function(aaa, (token_expires_xxx ~ true))',
      'test_function(test_function(aaa), (token_expired ~ true), (! is_empty(ccc)))',
      'test_function(aaa, bbb, (! ccc)) ~ (! (first_name IS "John Doe"))',
      'a ISNT is_empty(b)',
      '! length(first_name)',
      'aaa = (bb AND cc)',
      'first_name != age',
      'length(first_name) < is_empty(age)',
      'first_name < is_empty(age)',
    ];

    const invalidTypesOutput = [
      ['TypeError: Function "test_function" has wrong 2nd rest arg passed, should be TEXT but is BOOLEAN'],
      [
        'TypeError: Function "test_function" has wrong 1st rest arg passed, should be TEXT but is BOOLEAN',
        'TypeError: Function "test_function" has wrong 2nd rest arg passed, should be TEXT but is PHRASE',
        'TypeError: Function "test_function" has wrong 3rd rest arg passed, should be TEXT but is BOOLEAN',
      ],
      [
        'TypeError: Function \"test_function\" has wrong 3rd rest arg passed, should be TEXT but is PHRASE',
      ],
      [
        'TypeError: Both sides of ISNT expression should be of same type, but got LHS: TEXT and RHS: BOOLEAN.',
      ],
      [
        'TypeError: Operand of NOT operation should be a BOOLEAN, but got NUMBER',
      ],
      [
        'TypeError: Both sides of = expression should be of same type, but got LHS: TEXT and RHS: PHRASE.',
      ],
      [
        'TypeError: If both sides of != expression are model selectors, than their matching types should equal, but got LHS matching type: TEXT, RHS matching type: NUMBER.',
      ],
      [
        'TypeError: Both sides of < expression should be of same type, but got LHS: NUMBER and RHS: BOOLEAN.',
      ],
      [
        'TypeError: If LHS of < expression is model selector, than its matching type should equal RHS return type, but got LHS matching type: TEXT, RHS return type: BOOLEAN.',
      ],
    ];

    zip<any>(validInput, successfulOutputValues)
      .forEach(([input, output]) => {
        describe(`for valid input: ${input}`, () => {
          const parser = SearchQLParser.create({ model });
          const parsed = parser.parse(input);

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
        const parsed = SearchQLParser.create({ model }).parse(input);

        it('should return Left', () => {
          expect(parsed.isLeft(), parsed.cata(String, String)).to.be.true;
        });

        it('should provide original input', () => {
          expect(parsed.cata(
            f => f.map(failure => (failure as ParseFailure).query)[0],
            () => null)).to.equal(input);
        });

      });
    });

    invalidTypesInput.forEach((input, i) => {
      describe(`for syntactically valid yet wrongly typed input: ${input}`, () => {
        const parsed = SearchQLParser.create({ model }).parse(input);

        it('should return Left', () => {
          expect(parsed.isLeft(), parsed.cata(String, String)).to.be.true;
        });

        it('should yield proper type error information', () => {
          expect(parsed.left().map(String)).to.deep.equal(invalidTypesOutput[i]);
        });

      });
    });

  });

  describe('with custom config', () => {

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
        const parsed = SearchQLParser.create(configC).parse(input);

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
        const parsed = SearchQLParser.create(configC).parse(input);

        it('should return Left', () => {
          expect(parsed.isLeft()).to.be.true;
        });

        it('should provide original input', () => {
          expect(parsed.cata(
            f => f.map(failure => (failure as ParseFailure).query)[0],
            () => null)).to.equal(input);
        });

      });
    });

  });

});
