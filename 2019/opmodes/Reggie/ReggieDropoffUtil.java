package opmodes.Reggie;

import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.external.tfod.Recognition;

import java.util.List;

import team25core.DeadReckonPath;

import static opmodes.Reggie.ReggieDropoffUtil.MineralPosition.UNKNOWN;
import static team25core.MineralDetectionTask.LABEL_GOLD_MINERAL;

/**
 * Created by Lizzie on 2/6/2019.
 */
public class ReggieDropoffUtil {

    // groundwork for autonomous variables
    public enum StartingPosition {
        CRATER,
        DEPOT,
        DEFAULT,
    }

    public enum EndingPosition {
        PARKING,
        NOT_PARKING,
        DOUBLE_SAMPLING,
        DEFAULT,
    }

    public enum MineralPosition {
        LEFT,
        RIGHT,
        CENTER,
        UNKNOWN,
    }

    public enum HangingPosition {
        HANGING,
        NOT_HANGING,
        DEFAULT,
    }

    public static void sendPositionTelemetry(MineralPosition pos, Telemetry.Item item) {
        switch (pos) {
            case LEFT:
                item.setValue("LEFT");
                break;
            case RIGHT:
                item.setValue("RIGHT");
                break;
            case CENTER:
                item.setValue("CENTER");
                break;
        }
    }

    public static MineralPosition determineGoldPosition(List<Recognition> minerals) {
        if (minerals != null) {
            int goldMineralX = -1;
            int silverMineral1X = -1;
            int silverMineral2X = -1;
            for (Recognition recognition : minerals) {
                if (recognition.getLabel().equals(LABEL_GOLD_MINERAL)) {
                    goldMineralX = (int) recognition.getLeft();
                } else if (silverMineral1X == -1) {
                    silverMineral1X = (int) recognition.getLeft();
                } else {
                    silverMineral2X = (int) recognition.getLeft();
                }
            }

            // two mineral detection code
            if (goldMineralX != -1 && silverMineral1X != -1) {
                if (goldMineralX < silverMineral1X) {
                    return MineralPosition.CENTER;
                } else if (goldMineralX > silverMineral1X) {
                    return MineralPosition.RIGHT;
                }
            } else if (silverMineral1X != -1 && silverMineral2X != -1) {
                return MineralPosition.LEFT;
            }
            /*
            // three mineral detection code
            if (goldMineralX != -1 && silverMineral1X != -1 && silverMineral2X != -1) {
                if (goldMineralX < silverMineral1X && goldMineralX < silverMineral2X) {
                    return MineralPosition.LEFT;
                } else if (goldMineralX > silverMineral1X && goldMineralX > silverMineral2X) {
                    return MineralPosition.RIGHT;
                } else {
                    return MineralPosition.CENTER;
                }
            }
            */

        }
        return UNKNOWN;
    }

    DeadReckonPath[][][] paths = new DeadReckonPath[2][3][3];

    public ReggieDropoffUtil () {
        /*
        CRATER CODE: double sampling OR parking in 3 paths
        */
        // crater, double sampling, center mineral
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.CENTER.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 30.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 45.0, 0.5);

        // crater, parking, center
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 30.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 45.0, 0.5);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 15.0, 0.5);

        // crater, double sampling, left mineral path
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.LEFT.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 30.0, 0.3);

        // crater, parking, left mineral path
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 30.0, 0.3);

        // crater, double sampling, right mineral path
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.RIGHT.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 80.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 35.0, 0.5);

        // crater, parking, right mineral path
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 80.0, 0.3);
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 35.0, 0.5);

        /*
        DEPOT CODE: marker dropoff with parking
        */

        // depot, parking, center mineral
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 30.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 45.0, 0.5);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 48.0, 0.5);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 63.0, -0.5);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 14.0, 0.5);

        // depot, no parking, center mineral
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.CENTER.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 30.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 45.0, 0.5);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 48.0, 0.5);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 63.0, -0.5);

        // depot, parking, left mineral
       paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 30.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 48.0, 0.5);

        // depot, no parking, left mineral
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.LEFT.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 30.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 48.0, 0.5);

        // depot, parking, right mineral
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 80.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 35.0, 0.5);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 46.0, -0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 25.0, 0.5);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 30.0, -0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 16.0, 0.5);

        // depot, no parking, right mineral
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.RIGHT.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 80.0, 0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 35.0, 0.5);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 46.0, -0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 25.0, 0.5);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 30.0, -0.3);
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 16.0, 0.5);
    }

    public DeadReckonPath getPath (ReggieDropoffUtil.StartingPosition start, ReggieDropoffUtil.EndingPosition end, ReggieDropoffUtil.MineralPosition position) { return paths[start.ordinal()][end.ordinal()][position.ordinal()];
    }
}
