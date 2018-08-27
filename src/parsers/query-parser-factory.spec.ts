/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import { zip } from 'lodash';

import {
  AndOperator,
  BinaryOperationExpression,
  Expression,
  NotExpression,
  OrOperator,
  TextExpression,
} from '../ast';
import { SyntaxConfig } from '../config';
import { ParserName } from './names';
import { QueryParserFactory } from './query-parser-factory';

const { AND, EXACT_MATCHER, GROUP_END, GROUP_START, OR } = new SyntaxConfig();

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
        `a${AND}`,
        `(((${OR}a)))`,
        '"aa:bb AND cc dd"',
        // 'aa : AA',
        'aa AND bb',
        '(aa AND bb)',
        // 'aa AND (bb:BB OR cc) OR (dd AND (ee ~ EE OR ff))',
        'aaa AND bbb OR ccc AND ddd',
        'NOT abc',
        'aa AND NOT bb',
        'aa NOT bb',
        'NOT aaa AND NOT bbb',
        'NOT (aaa OR bbb) AND ccc',
        // 'aaa AND NOT bb ~ BB',
        'aaa AND (bbb OR ccc) NOT ddd',
        '(aaa OR bbb) NOT ccc',
        'aaa AND bbb AND ccc NOT ddd',
      ];

      const validOutput = [
        new TextExpression('aa'),
        new TextExpression(`a${AND}`),
        new TextExpression(`${OR}a`),
        new TextExpression('aa:bb AND cc dd'),
        // new LabelledExpression('aa', new TextExpression('AA')),
        new BinaryOperationExpression(AndOperator.one, [
          new TextExpression('aa'),
          new TextExpression('bb'),
        ]),
        new BinaryOperationExpression(AndOperator.one, [
          new TextExpression('aa'),
          new TextExpression('bb'),
        ]),
        // new BinaryOperationExpression(OrOperator.one, [
        //   new BinaryOperationExpression(AndOperator.one, [
        //     new TextExpression('aa'),
        //     new BinaryOperationExpression(OrOperator.one, [
        //       new LabelledExpression('bb', new TextExpression('BB')),
        //       new TextExpression('cc'),
        //     ]),
        //   ]),
        //   new BinaryOperationExpression(AndOperator.one, [
        //     new TextExpression('dd'),
        //     new BinaryOperationExpression(OrOperator.one, [
        //       new LabelledExpression('ee', new TextExpression('EE')),
        //       new TextExpression('ff'),
        //     ]),
        //   ]),
        // ]),
        new BinaryOperationExpression(AndOperator.one, [
          new BinaryOperationExpression(OrOperator.one, [
            new BinaryOperationExpression(AndOperator.one, [
              new TextExpression('aaa'),
              new TextExpression('bbb'),
            ]),
            new TextExpression('ccc'),
          ]),
          new TextExpression('ddd'),
        ]),
        new NotExpression(new TextExpression('abc')),
        new BinaryOperationExpression(AndOperator.one, [
          new TextExpression('aa'),
          new NotExpression(new TextExpression('bb')),
        ]),
        new BinaryOperationExpression(AndOperator.one, [
          new TextExpression('aa'),
          new NotExpression(new TextExpression('bb')),
        ]),
        new BinaryOperationExpression(AndOperator.one, [
          new NotExpression(new TextExpression('aaa')),
          new NotExpression(new TextExpression('bbb')),
        ]),
        new BinaryOperationExpression(AndOperator.one, [
          new NotExpression(new BinaryOperationExpression(OrOperator.one, [
            new TextExpression('aaa'),
            new TextExpression('bbb'),
          ])),
          new TextExpression('ccc'),
        ]),
        // new BinaryOperationExpression(AndOperator.one, [
        //   new TextExpression('aaa'),
        //   new NotExpression(new LabelledExpression('bb', new TextExpression('BB'))),
        // ]),
        new BinaryOperationExpression(AndOperator.one, [
          new BinaryOperationExpression(AndOperator.one, [
            new TextExpression('aaa'),
            new BinaryOperationExpression(OrOperator.one, [
              new TextExpression('bbb'),
              new TextExpression('ccc'),
            ]),
          ]),
          new NotExpression(new TextExpression('ddd')),
        ]),
        new BinaryOperationExpression(AndOperator.one, [
          new BinaryOperationExpression(OrOperator.one, [
            new TextExpression('aaa'),
            new TextExpression('bbb'),
          ]),
          new NotExpression(new TextExpression('ccc')),
        ]),
        new BinaryOperationExpression(AndOperator.one, [
          new BinaryOperationExpression(AndOperator.one, [
            new BinaryOperationExpression(AndOperator.one, [
              new TextExpression('aaa'),
              new TextExpression('bbb'),
            ]),
            new TextExpression('ccc'),
          ]),
          new NotExpression(new TextExpression('ddd')),
        ]),
      ];

      const invalidInput = [
        'ASDfas 32%@$%4512 u954anna as d][;];.{P} AND',
        'OR AND NOT (OR AND NOT) asd: asd not ASD:ASd',
        ' ',
        AND,
        OR,
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
      ];

      const validOutput = [
        new TextExpression('aa'),
      ];

      const invalidInput = [
        'desc abc AND',
        'NOT aaa',
      ];

      test(validInput, validOutput, invalidInput, subsetOfAllParsers);

    });

  });

});
