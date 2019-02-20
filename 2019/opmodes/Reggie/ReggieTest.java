package opmodes;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.Telemetry;

import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RunToEncoderValueTask;
import team25core.SingleShotTimerTask;

/**
 * Created by Lizzie on 2/16/2019.
 */
@Autonomous(name = "Reggie Autonomous")
public class ReggieTest extends Robot {

    // drivetrain variables
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private FourWheelDirectDrivetrain drivetrain;

    // collection variables
    private DcMotor fourBar;
    private DcMotor bungeeBox;

    // lift variables
    private DcMotor liftLeft;
    private DcMotor liftRight;

    // gamepad variables
    private GamepadTask gamepad1;
    private GamepadTask gamepad2;

    // telemetry items
    private Telemetry.Item startingPositionItem;
    private Telemetry.Item hangingItem;
    private Telemetry.Item endingPositionItem;
    private Telemetry.Item goldMineralItem;
    private Telemetry.Item doubleSampling;

    // path variables
    private boolean hangingBoolean;
    private ReggieDropoffUtil dropoffUtil;
    private DeadReckonPath knockPath;
    private ReggieExitDepotDropoff exitDepotDropoff;
    private DeadReckonPath exitDepotPath;
    private ReggieDropoffUtil.StartingPosition robotStartingPosition;
    private ReggieDropoffUtil.MineralPosition goldMineralPosition;
    private ReggieDropoffUtil.EndingPosition robotEndingPosition;

    @Override
    public void init() {
        // drivetrain initialization
        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");
        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);
        drivetrain.encodersOn();
        drivetrain.resetEncoders();

        // collection variables
        fourBar = hardwareMap.dcMotor.get("fourBar");
        fourBar.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        fourBar.setMode(DcMotor.RunMode.RESET_ENCODERS);
        bungeeBox = hardwareMap.dcMotor.get("bungeeBox");
        bungeeBox.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        bungeeBox.setMode(DcMotor.RunMode.RESET_ENCODERS);

        // lift variables
        liftLeft = hardwareMap.dcMotor.get("liftLeft");
        liftLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        liftLeft.setMode(DcMotor.RunMode.RESET_ENCODERS);
        liftRight = hardwareMap.dcMotor.get("liftRight");
        liftRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        liftRight.setMode(DcMotor.RunMode.RESET_ENCODERS);

        // initializing gamepad variables
        gamepad1 = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad1);
        gamepad2 = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_2);
        addTask(gamepad2);

        // initializing telemetry items
        goldMineralItem = telemetry.addData("Gold Mineral Position: ", "NOT SELECTED");
        startingPositionItem = telemetry.addData("Starting Position", "NOT SELECTED");
        hangingItem = telemetry.addData("Hanging: ", "NOT SELECTED");
        endingPositionItem = telemetry.addData("Ending Position: ", "NOT SELECTED");

        // path variables
        dropoffUtil = new ReggieDropoffUtil();
        knockPath = new DeadReckonPath();
        exitDepotDropoff = new ReggieExitDepotDropoff();
        exitDepotPath = new DeadReckonPath();
        robotStartingPosition = ReggieDropoffUtil.StartingPosition.DEFAULT;
        goldMineralPosition = ReggieDropoffUtil.MineralPosition.UNKNOWN;
        robotEndingPosition = ReggieDropoffUtil.EndingPosition.DEFAULT;
    }


    @Override
    public void start() {
        exitDepotPath = exitDepotDropoff.getPath(goldMineralPosition);
        initialMove(knockPath);
    }

    @Override
    public void handleEvent(RobotEvent e) {
        if (e instanceof GamepadTask.GamepadEvent) {
            // starting position, hanging, parking, gold mineral position, double sampling
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;
            switch (event.kind) {
                case BUTTON_X_DOWN:
                    endingPositionItem.setValue("PARKING");
                    break;
                case BUTTON_Y_DOWN:
                    endingPositionItem.setValue("NOT PARKING");
                    robotEndingPosition = ReggieDropoffUtil.EndingPosition.NOT_PARKING;
                    break;
                case BUTTON_A_DOWN:
                    endingPositionItem.setValue("DOUBLE SAMPLING");
                    robotEndingPosition = ReggieDropoffUtil.EndingPosition.DOUBLE_SAMPLING;
                    break;
                case RIGHT_BUMPER_DOWN:
                    startingPositionItem.setValue("DEPOT");
                    robotStartingPosition = ReggieDropoffUtil.StartingPosition.DEPOT;
                    break;
                case RIGHT_TRIGGER_DOWN:
                    startingPositionItem.setValue("CRATER");
                    robotStartingPosition = ReggieDropoffUtil.StartingPosition.CRATER;
                    break;
                case LEFT_BUMPER_DOWN:
                    hangingItem.setValue("HANGING");
                    hangingBoolean = true;
                    break;
                case LEFT_TRIGGER_DOWN:
                    hangingItem.setValue("NOT HANGING");
                    hangingBoolean = false;
                    break;
            }
        }
    }

    public void unlatch() {
        RunToEncoderValueTask leftTask = new RunToEncoderValueTask(this, liftLeft, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_LEFT_UP) {
            @Override
            public void handleEvent(RobotEvent event) {
                if (((RunToEncoderValueEvent) event).kind == EventKind.DONE) {
                    liftLeft.setPower(0.0);
                }
            }
        };
        RunToEncoderValueTask rightTask = new RunToEncoderValueTask(this, liftRight, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_RIGHT_UP) {
            @Override
            public void handleEvent(RobotEvent event) {
                if (((RunToEncoderValueEvent) event).kind == EventKind.DONE) {
                    liftRight.setPower(0.0);
                }
            }
        };
        addTask(leftTask);
        addTask(rightTask);
    }

    protected void initialMove(final DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        // depot = markerDrop()
                        break;
                }
            }
        });
    }

    protected void markerDrop() {
        RobotLog.i("Carrying out marker drop method.");
        addTask(new SingleShotTimerTask(this, 600) {
            @Override
            public void handleEvent(RobotEvent e) {
                exitDepot(exitDepotPath);
            }
        });
    }

    protected void exitDepot(final DeadReckonPath path) {
        RobotLog.i("Carrying out exit depot method.");
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent (RobotEvent e) {
                DeadReckonTask.DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        RobotLog.i("251: You're done, congrats!");
                }
            }
        });
    }
}
