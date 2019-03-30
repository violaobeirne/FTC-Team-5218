package opmodes.Houston;

import opmodes.Houston.HoustonDropoffUtil;
import opmodes.Houston.HoustonBachAutonomous;
import team25core.DeadReckonPath;

/**
 * Created by Lizzie on 11/27/2018.
 */
public class HoustonExitDepotDropoff {

        HoustonDropoffUtil.StartingPosition StartingPosition = HoustonBachAutonomous.robotStartingPosition;
        HoustonDropoffUtil.EndingPosition EndingPosition = HoustonBachAutonomous.robotEndingPosition;
        HoustonDropoffUtil.MineralPosition MineralPosition = HoustonBachAutonomous.goldMineralPosition;

        DeadReckonPath[][][] paths = new DeadReckonPath[2][2][3];
    public HoustonExitDepotDropoff() {
        /*
        CRATER CODE: parking in 3 paths
        */
        // crater, parking, center
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()] = new DeadReckonPath();

        // crater, parking, left mineral path
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()] = new DeadReckonPath();

        // crater, parking, right mineral path
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()] = new DeadReckonPath();

        /*
        DEPOT CODE: marker dropoff with parking
        */

        // depot, parking, center mineral
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 76.0, 0.5);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 100.0, -0.8);

        // depot, no parking, center mineral
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.CENTER.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 15.0, -0.5);

        // depot, parking, left mineral
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()] = new DeadReckonPath();

        // parking portion
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 9.0, 0.5);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 40.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 30.0, 0.5);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 100.0, -0.5);

        // depot, no parking, left mineral
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.LEFT.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 15.0, -0.5);

        // depot, parking, right mineral
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 60.0, 0.5);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 100.0, -0.8);

        // depot, no parking, right mineral
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.RIGHT.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 15.0, -0.5);

    }

    public DeadReckonPath getPath (HoustonDropoffUtil.StartingPosition start, HoustonDropoffUtil.EndingPosition end, HoustonDropoffUtil.MineralPosition position)
    {
        return paths[start.ordinal()][end.ordinal()][position.ordinal()];
    }
}
