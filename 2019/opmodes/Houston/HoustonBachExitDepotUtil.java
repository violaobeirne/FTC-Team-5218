package opmodes.Houston;

import team25core.DeadReckonPath;

/**
 * Created by Lizzie on 2/6/2019.
 */
public class HoustonBachExitDepotUtil {

    DeadReckonPath[][] paths = new DeadReckonPath[2][3];

    public HoustonBachExitDepotUtil() {
        /*
        DEPOT CODE: marker dropoff with parking
        */

        // depot, parking, center mineral
        paths[HoustonBachDropoffUtil.EndingPosition.PARKING.ordinal()][HoustonBachDropoffUtil.MineralPosition.CENTER.ordinal()] = new DeadReckonPath();
        paths[HoustonBachDropoffUtil.EndingPosition.PARKING.ordinal()][HoustonBachDropoffUtil.MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5.0, -0.5);
        paths[HoustonBachDropoffUtil.EndingPosition.PARKING.ordinal()][HoustonBachDropoffUtil.MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 60.0, -0.5);
        paths[HoustonBachDropoffUtil.EndingPosition.PARKING.ordinal()][HoustonBachDropoffUtil.MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 85.0, 0.7);

        // depot, no parking, center mineral
        paths[HoustonBachDropoffUtil.EndingPosition.NOT_PARKING.ordinal()][HoustonBachDropoffUtil.MineralPosition.CENTER.ordinal()] = new DeadReckonPath();
        paths[HoustonBachDropoffUtil.EndingPosition.NOT_PARKING.ordinal()][HoustonBachDropoffUtil.MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10.0, -0.5);

        // depot, parking, left mineral
        paths[HoustonBachDropoffUtil.EndingPosition.PARKING.ordinal()][HoustonBachDropoffUtil.MineralPosition.LEFT.ordinal()] = new DeadReckonPath();
        paths[HoustonBachDropoffUtil.EndingPosition.PARKING.ordinal()][HoustonBachDropoffUtil.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 80.0, -0.8);

        // depot, no parking, left mineral
        paths[HoustonBachDropoffUtil.EndingPosition.NOT_PARKING.ordinal()][HoustonBachDropoffUtil.MineralPosition.LEFT.ordinal()] = new DeadReckonPath();
        paths[HoustonBachDropoffUtil.EndingPosition.NOT_PARKING.ordinal()][HoustonBachDropoffUtil.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10.0, -0.5);

        // depot, parking, right mineral
        paths[HoustonBachDropoffUtil.EndingPosition.PARKING.ordinal()][HoustonBachDropoffUtil.MineralPosition.RIGHT.ordinal()] = new DeadReckonPath();
        paths[HoustonBachDropoffUtil.EndingPosition.PARKING.ordinal()][HoustonBachDropoffUtil.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 80.0, -0.8);

        // depot, no parking, right mineral
        paths[HoustonBachDropoffUtil.EndingPosition.NOT_PARKING.ordinal()][HoustonBachDropoffUtil.MineralPosition.RIGHT.ordinal()] = new DeadReckonPath();
        paths[HoustonBachDropoffUtil.EndingPosition.NOT_PARKING.ordinal()][HoustonBachDropoffUtil.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10.0, -0.5);
    }

    public DeadReckonPath getPath (HoustonBachDropoffUtil.EndingPosition end, HoustonBachDropoffUtil.MineralPosition position) { return paths[end.ordinal()][position.ordinal()];
    }
}
