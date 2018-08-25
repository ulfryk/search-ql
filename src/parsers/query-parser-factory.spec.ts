/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import { zip } from 'lodash';

import {
  AndOperator,
  BasicExpression,
  BinaryOperationExpression,
  Expression,
  LabelledExpression,
  NotExpression,
  OrOperator,
} from '../ast';
import { SyntaxConfig } from '../config';
import { ParserName } from './names';
import { QueryParserFactory } from './query-parser-factory';

const { AND, EXACT_MATCHER, GROUP_END, GROUP_START, LABEL_DELIMITER, OR } = new SyntaxConfig();

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
        [ParserName.Basic, ParserName.JoinedGroup, ParserName.Labelled, ParserName.Not];

      const validInput = [
        'aa',
        `a${AND}`,
        `(((${OR}a)))`,
        '"aa:bb AND cc dd"',
        'aa : AA',
        'aa AND bb',
        '(aa AND bb)',
        'aa AND (bb:BB OR cc) OR (dd AND (ee:EE OR ff))',
        'aaa AND bbb OR ccc AND ddd',
        'NOT abc',
        'aa AND NOT bb',
        'aa NOT bb',
        'NOT aaa AND NOT bbb',
        'NOT (aaa OR bbb) AND ccc',
        'aaa AND NOT bb:BB',
        'aaa AND (bbb OR ccc) NOT ddd',
        '(aaa OR bbb) NOT ccc',
        'aaa AND bbb AND ccc NOT ddd',
      ];

      const validOutput = [
        new BasicExpression('aa'),
        new BasicExpression(`a${AND}`),
        new BasicExpression(`${OR}a`),
        new BasicExpression('aa:bb AND cc dd'),
        new LabelledExpression('aa', new BasicExpression('AA')),
        new BinaryOperationExpression(AndOperator.one, [
          new BasicExpression('aa'),
          new BasicExpression('bb'),
        ]),
        new BinaryOperationExpression(AndOperator.one, [
          new BasicExpression('aa'),
          new BasicExpression('bb'),
        ]),
        new BinaryOperationExpression(OrOperator.one, [
          new BinaryOperationExpression(AndOperator.one, [
            new BasicExpression('aa'),
            new BinaryOperationExpression(OrOperator.one, [
              new LabelledExpression('bb', new BasicExpression('BB')),
              new BasicExpression('cc'),
            ]),
          ]),
          new BinaryOperationExpression(AndOperator.one, [
            new BasicExpression('dd'),
            new BinaryOperationExpression(OrOperator.one, [
              new LabelledExpression('ee', new BasicExpression('EE')),
              new BasicExpression('ff'),
            ]),
          ]),
        ]),
        new BinaryOperationExpression(AndOperator.one, [
          new BinaryOperationExpression(OrOperator.one, [
            new BinaryOperationExpression(AndOperator.one, [
              new BasicExpression('aaa'),
              new BasicExpression('bbb'),
            ]),
            new BasicExpression('ccc'),
          ]),
          new BasicExpression('ddd'),
        ]),
        new NotExpression(new BasicExpression('abc')),
        new BinaryOperationExpression(AndOperator.one, [
          new BasicExpression('aa'),
          new NotExpression(new BasicExpression('bb')),
        ]),
        new BinaryOperationExpression(AndOperator.one, [
          new BasicExpression('aa'),
          new NotExpression(new BasicExpression('bb')),
        ]),
        new BinaryOperationExpression(AndOperator.one, [
          new NotExpression(new BasicExpression('aaa')),
          new NotExpression(new BasicExpression('bbb')),
        ]),
        new BinaryOperationExpression(AndOperator.one, [
          new NotExpression(new BinaryOperationExpression(OrOperator.one, [
            new BasicExpression('aaa'),
            new BasicExpression('bbb'),
          ])),
          new BasicExpression('ccc'),
        ]),
        new BinaryOperationExpression(AndOperator.one, [
          new BasicExpression('aaa'),
          new NotExpression(new LabelledExpression('bb', new BasicExpression('BB'))),
        ]),
        new BinaryOperationExpression(AndOperator.one, [
          new BinaryOperationExpression(AndOperator.one, [
            new BasicExpression('aaa'),
            new BinaryOperationExpression(OrOperator.one, [
              new BasicExpression('bbb'),
              new BasicExpression('ccc'),
            ]),
          ]),
          new NotExpression(new BasicExpression('ddd')),
        ]),
        new BinaryOperationExpression(AndOperator.one, [
          new BinaryOperationExpression(OrOperator.one, [
            new BasicExpression('aaa'),
            new BasicExpression('bbb'),
          ]),
          new NotExpression(new BasicExpression('ccc')),
        ]),
        new BinaryOperationExpression(AndOperator.one, [
          new BinaryOperationExpression(AndOperator.one, [
            new BinaryOperationExpression(AndOperator.one, [
              new BasicExpression('aaa'),
              new BasicExpression('bbb'),
            ]),
            new BasicExpression('ccc'),
          ]),
          new NotExpression(new BasicExpression('ddd')),
        ]),
      ];

      const invalidInput = [
        'ASDfas 32%@$%4512 u954anna as d][;];.{P} AND',
        'OR AND NOT (OR AND NOT) asd: asd not ASD:ASd',
        ' ',
        AND,
        OR,
        `123${LABEL_DELIMITER}a${LABEL_DELIMITER}sd`,
        `${LABEL_DELIMITER}text`,
        `${GROUP_START}asd`,
        `asd${GROUP_END}`,
        `dasd${EXACT_MATCHER}a`,
      ];

      test(validInput, validOutput, invalidInput, allParserNames);

    });

    describe('with some parsers excluded', () => {

      const subsetOfAllParsers = [ParserName.Basic, ParserName.JoinedGroup];

      const validInput = [
        'aa',
      ];

      const validOutput = [
        new BasicExpression('aa'),
      ];

      const invalidInput = [
        'desc:abc',
        'NOT aaa',
      ];

      test(validInput, validOutput, invalidInput, subsetOfAllParsers);

    });

  });

});
