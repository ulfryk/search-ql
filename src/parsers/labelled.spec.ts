/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import * as _ from 'lodash';
import { Maybe, None, Some } from 'monet';

import { BasicExpression, Expression, fromPairs, LabelledExpression } from '../expressions';
import {
  AND,
  EXACT_MATCHER,
  GROUP_END,
  GROUP_START,
  LABEL_DELIMITER,
  LogicOperator,
  OR,
} from '../syntax-config';
import { labelledExpression } from './labelled';

describe('SearchQL parsers', () => {

  const l = (label: string, content: string) =>
    [label, LABEL_DELIMITER, content].join('');

  const lg = (label: string, content: string) =>
    l(label, [GROUP_START, content, GROUP_END].join(''));

  const lo = (labelValue: string, expression: Expression) =>
    new LabelledExpression(labelValue, expression);

  const validInput = [
    l('aa', 'AA'),
    l('lorem ', ' ipsum'),
    l('a', '"ASDfas 32%@$%4512 u954anna as d][;];.{P} AND"'),
    l('bv_cb', '"OR AND NOT (OR AND NOT) asd: asd not ASD:ASd"'),
    l('hello.dot.com', '" "'),
    l('blue', ' 123123123'),
    l('red', 's#$%^876gsbjh_-s-S-s'),
    l('and', AND + 'a'),
    l('or', OR + 'a'),
    l(AND + 'a', 'and'),
    l(OR + 'a', 'or'),
    lg('lo12_rem ', 'ispum'),
    lg('valid', ` lorem ${AND} ispum`),
    lg('world', ` "ASDfas 32%@$%4512 u954anna as d][;];.{P} AND" ${OR} shgfghjfhjfghs `),
    lg('sit.Amet', ` \n lorem\n ${OR} 's#$%^876gsbjh_-s-S-s' ${AND} "OR AND(OR AND) asd:ASd"`),
  ];

  const validOutput = [
    lo('aa', new BasicExpression('AA')),
    lo('lorem', new BasicExpression('ipsum')),
    lo('a', new BasicExpression('ASDfas 32%@$%4512 u954anna as d][;];.{P} AND')),
    lo('bv_cb', new BasicExpression('OR AND NOT (OR AND NOT) asd: asd not ASD:ASd')),
    lo('hello.dot.com', new BasicExpression(' ')),
    lo('blue', new BasicExpression('123123123')),
    lo('red', new BasicExpression('s#$%^876gsbjh_-s-S-s')),
    lo('and', new BasicExpression(AND + 'a')),
    lo('or', new BasicExpression(OR + 'a')),
    lo(AND + 'a', new BasicExpression('and')),
    lo(OR + 'a', new BasicExpression('or')),
    lo('lo12_rem', fromPairs([
      [None<LogicOperator>(), new BasicExpression('ispum')],
    ] as [Maybe<LogicOperator>, Expression][])),
    lo('valid', fromPairs([
      [None<LogicOperator>(), new BasicExpression('lorem')] as [Maybe<LogicOperator>, Expression],
      [Some(AND), new BasicExpression('ispum')],
    ] as [Maybe<LogicOperator>, Expression][])),
    lo('world', fromPairs([
      [
        None<LogicOperator>(),
        new BasicExpression('ASDfas 32%@$%4512 u954anna as d][;];.{P} AND'),
      ] as [Maybe<LogicOperator>, Expression],
      [Some(OR), new BasicExpression('shgfghjfhjfghs')],
    ] as [Maybe<LogicOperator>, Expression][])),
    lo('sit.Amet', fromPairs([
      [None<LogicOperator>(), new BasicExpression('lorem')] as [Maybe<LogicOperator>, Expression],
      [Some(OR), new BasicExpression('\'s#$%^876gsbjh_-s-S-s\'')],
      [Some(AND), new BasicExpression('OR AND(OR AND) asd:ASd')],
    ] as [Maybe<LogicOperator>, Expression][])),
  ];

  const invalidInput = [
    'ASDfas 32%@$%4512 u954anna as d][;];.{P} AND',
    'OR AND NOT (OR AND NOT) asd: asd not ASD:ASd',
    l('label', 'lorem ipsum'),
    l('', '"  "'),
    l('aa', '  '),
    l('and', AND),
    l('or', OR),
    l('label', `123${LABEL_DELIMITER}asd`),
    l('label', `asd${LABEL_DELIMITER}123`),
    l('label', `dasd${EXACT_MATCHER}a`),
    l(`hello${LABEL_DELIMITER}asd`, '" "'),
    l(AND, ' 123123123'),
    l(OR, 's#$%^876gsbjh_-s-S-s'),
    l(EXACT_MATCHER, 's#$%^876gsbjh_-s-S-s'),
    lg('label', 'lorem ipsum'),
    lg('label', `lorem ${AND}`),
    lg('label', `lorem${AND} ispum`),
    lg('label', `dolor ${validInput[0]}`),
    lg('label', `dolor${validInput[0]}`),
    lg('label', `dolor ${AND}${validInput[0]}`),
  ];

  describe('labelledExpression', () => {

    _.zip<any>(validInput, validOutput).forEach(([input, output]) => {
      describe(`for valid input: '${input}'`, () => {
        const parsed = labelledExpression.parse(input);

        it('should succeed', () => {
          expect(parsed.status).to.be.true;
        });

        it('should provide proper value', () => {
          expect(parsed.status ? parsed.value : null).to.deep.equal(output);
        });

      });
    });

    invalidInput.forEach(input => {
      describe(`for invalid input: '${input}'`, () => {

        it('should fail', () => {
          expect(labelledExpression.parse(input).status).to.be.false;
        });

      });
    });

  });

});
