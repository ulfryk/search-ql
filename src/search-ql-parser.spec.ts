/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import { Map } from 'immutable';
import { zip } from 'lodash';
import { Either } from 'monet';

import { AndOperator, FunctionExpression, LikeOperator, OrOperator } from './ast';
import { ParseFailure, ValueType } from './common/model';
import { SearchQLParser } from './search-ql-parser';
import { and, config, fn, isNotR, isR, like, likeR, not, notLike, or, phrase, txt } from './testing/utils';

describe('SearchQL', () => {

  describe('SearchQLParser with regular config', () => {

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
            f => f.map(({ query }: ParseFailure) => query)[0],
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

  describe('SearchQLParser with regular config and defined model', () => {

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
      'is_empty(first_name)',
      '! is_empty(first_name)',
      'first_name !~ noone',
    ];

    const successfulOutputValues = [
      and(phrase('aaa'), phrase('bbb')),
      like(txt('token_expired'), txt('true')),
      or(like(txt('first_name'), txt('Adam')), like(txt('token_expired'), txt('true'))),
      fn('test_function')(txt('aaa'), txt('b(b & b)')),
      and(isR(txt('aaa'), txt('bbb')), isNotR(txt('cc'), txt('dd'))),
      FunctionExpression.fromParseResult(config.functions.get('is_empty'), [txt('first_name')]),
      not(FunctionExpression.fromParseResult(config.functions.get('is_empty'), [txt('first_name')])),
      notLike(txt('first_name'), txt('noone')),
    ].map(Either.of);

    const successfulOutputEvaluations = [
      true,
      false,
      true,
      false,
      false,
      false,
      true,
      true,
    ];

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
      '! length(first_name)',
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
        'TypeError: Operand of NOT operation should be a BOOLEAN, but got NUMBER',
      ],
    ];

    const entity = Map<string, string>({
      age: '55',
      first_name: 'Adam',
      last_name: 'aaa bbb',
      token_expired: 'false',
    });

    zip<any>(validInput, successfulOutputValues, successfulOutputEvaluations)
      .forEach(([input, output, evaluation]) => {
        describe(`for valid input: ${input}`, () => {
          const parser = SearchQLParser.create({ model });
          const parsed = parser.parse(input);
          const tested = parser.toTester(parsed).map(__ => __.test(entity));

          it('should return Right', () => {
            expect(parsed.isRight(), parsed.cata(String, String)).to.be.true;
          });

          it('should return parsed expression', () => {
            expect(parsed.equals(output)).to.be.true;
          });

          it('should properly evaluate expression against input object', () => {
            expect(tested.right().value).to.equal(evaluation);
          });

        });
      });

    invalidInput.forEach(input => {
      describe(`for invalid input: ${input}`, () => {
        const parsed = SearchQLParser.create({ model }).parse(input);

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

  describe('SearchQLParser with custom config', () => {

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
            f => f.map(({ query }: ParseFailure) => query)[0],
            () => null)).to.equal(input);
        });

      });
    });

  });

});
