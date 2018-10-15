/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { zip } from 'lodash';

import { TimeFrame } from '../model';
import { parseDate } from './parse-date';

const date1 = new Date('2018-10-14T00:59:08.455Z').toJSON();
const date2 = new Date('2018-07-04T14:02:51.678Z').toISOString();
const date3 = new Date('2018-10-15T13:21:11.001Z').toDateString();

const input = [
  '2018-11-21',
  date1,
  date2,
  date3,
  'Aug 28 2018',
  '28 Aug 2018',
  'August 28, 2018',
  'Jan 28, 2018 20:15',
];

const output = [
  new TimeFrame(Date.parse('2018-11-20T23:00:00.000Z'), Date.parse('2018-11-21T22:59:59.999Z'), input[0]),
  new TimeFrame(Date.parse(date1), Date.parse(date1), input[1]),
  new TimeFrame(Date.parse(date2), Date.parse(date2), input[2]),
  new TimeFrame(Date.parse('2018-10-14T22:00:00.000Z'), Date.parse('2018-10-15T21:59:59.999Z'), input[3]),
  new TimeFrame(Date.parse('2018-08-27T22:00:00.000Z'), Date.parse('2018-08-28T21:59:59.999Z'), input[4]),
  new TimeFrame(Date.parse('2018-08-27T22:00:00.000Z'), Date.parse('2018-08-28T21:59:59.999Z'), input[5]),
  new TimeFrame(Date.parse('2018-08-27T22:00:00.000Z'), Date.parse('2018-08-28T21:59:59.999Z'), input[6]),
  new TimeFrame(Date.parse('2018-01-28T19:15:00.000Z'), Date.parse('2018-01-28T19:15:00.999Z'), input[7]),
];

describe('SearchQL expressions', () => {

  describe('parseDate', () => {

    it('should be a function', () => {
      expect(parseDate).to.be.instanceof(Function);
    });

    zip(input, output).forEach(([aDate, aFrame]) => {
      it(`should return proper time frame for string that represents a Date: "${aDate}"`, () => {
        const parsed = parseDate(aDate);
        const failInfo = `The "${aDate}" should parse to ${aFrame} but got ${parsed}`;
        expect(parsed.equals(aFrame), failInfo).to.be.true;
      });
    });

  });

});
