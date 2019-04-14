package opmodes.Houston;

import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.external.tfod.Recognition;

import java.util.List;

import team25core.DeadReckonPath;

import static opmodes.Houston.HoustonDropoffUtil.MineralPosition.UNKNOWN;
import static team25core.MineralDetectionTask.LABEL_GOLD_MINERAL;

/**
 * Created by Lizzie on 2/6/2019.
 */
public class HoustonExitDepotUtil {

    DeadReckonPath[][] paths = new DeadReckonPath[2][3];

    public HoustonExitDepotUtil() {
        /*
        DEPOT CODE: marker dropoff with parking
        */

        // depot, parking, center mineral
        paths[HoustonDropoffUtil.EndingPosition.PARKING.ordinal()][HoustonDropoffUtil.MineralPosition.CENTER.ordinal()] = new DeadReckonPath();
        paths[HoustonDropoffUtil.EndingPosition.PARKING.ordinal()][HoustonDropoffUtil.MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5.0, -0.5);
        paths[HoustonDropoffUtil.EndingPosition.PARKING.ordinal()][HoustonDropoffUtil.MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 60.0, -0.5);
        paths[HoustonDropoffUtil.EndingPosition.PARKING.ordinal()][HoustonDropoffUtil.MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 85.0, 0.7);

        // depot, no parking, center mineral
        paths[HoustonDropoffUtil.EndingPosition.NOT_PARKING.ordinal()][HoustonDropoffUtil.MineralPosition.CENTER.ordinal()] = new DeadReckonPath();
        paths[HoustonDropoffUtil.EndingPosition.NOT_PARKING.ordinal()][HoustonDropoffUtil.MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10.0, -0.5);

        // depot, parking, left mineral
        paths[HoustonDropoffUtil.EndingPosition.PARKING.ordinal()][HoustonDropoffUtil.MineralPosition.LEFT.ordinal()] = new DeadReckonPath();
        paths[HoustonDropoffUtil.EndingPosition.PARKING.ordinal()][HoustonDropoffUtil.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 80.0, -0.8);

        // depot, no parking, left mineral
        paths[HoustonDropoffUtil.EndingPosition.NOT_PARKING.ordinal()][HoustonDropoffUtil.MineralPosition.LEFT.ordinal()] = new DeadReckonPath();
        paths[HoustonDropoffUtil.EndingPosition.NOT_PARKING.ordinal()][HoustonDropoffUtil.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10.0, -0.5);

        // depot, parking, right mineral
        paths[HoustonDropoffUtil.EndingPosition.PARKING.ordinal()][HoustonDropoffUtil.MineralPosition.RIGHT.ordinal()] = new DeadReckonPath();
        paths[HoustonDropoffUtil.EndingPosition.PARKING.ordinal()][HoustonDropoffUtil.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20.0, -0.8);

        // depot, no parking, right mineral
        paths[HoustonDropoffUtil.EndingPosition.NOT_PARKING.ordinal()][HoustonDropoffUtil.MineralPosition.RIGHT.ordinal()] = new DeadReckonPath();
        paths[HoustonDropoffUtil.EndingPosition.NOT_PARKING.ordinal()][HoustonDropoffUtil.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10.0, -0.5);
    }

    public DeadReckonPath getPath (HoustonDropoffUtil.EndingPosition end, HoustonDropoffUtil.MineralPosition position) { return paths[end.ordinal()][position.ordinal()];
    }
}
